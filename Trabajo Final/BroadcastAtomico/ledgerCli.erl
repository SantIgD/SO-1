-module(ledgerCli).

%%Funciones de control
-export([startCli/0,stopCli/0]).

%%Libreria de acceso
-export([lGet/0,lAppend/1]).

%%Funciones del cliente del ledger
-export([send_tcpInit/1,clienteInfoInit/2,receive_tcpInit/1]).

%%Direccion ip del server
-define(Dir, "localhost").

%%Puerto del server
-define(Puerto, 1237).

%%Tiempo de espera del RECV
-define(TIMEOUT, 1000).


%
%%startCli : Se conecta con el servidor y spawnea/registra los procesos 
%            que hacen funcionar el cliente
startCli()->
    
    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
        {ok, Socket} ->
            io:format("Se pudo conectar con el servidor de ledger.~n"),  
            register(clienteInfo  , spawn(?MODULE, clienteInfoInit   , [0, Socket])),
            register(send_tcp     , spawn(?MODULE, send_tcpInit   , [Socket])),
            register(receive_tcp  , spawn(?MODULE, receive_tcpInit, [Socket]));

        {error, Reason} -> 
            io:format("No se conecto: ~p~nSi desea volvere a iniciar el cliente utilice la funcion starCli().~n",[Reason])
    end,    
    ok.

clienteInfoInit(N, Socket) ->
    process_flag(trap_exit, true),
    clienteInfo(N, Socket).
%
%%clienteInfo : Se encarga de contar la cantidad de requests hechos
%
clienteInfo(N, Socket)->
    receive
        {opRequest, PID} -> 
            Num = N + 1,
            PID ! {opAck, Num},
            clienteInfo(Num, Socket);

        {fin, PId} -> 
            
            unregister(clienteInfo),
            PId ! {contFinOk},
            cerrarSocket(Socket);

        {'EXIT', _Exiting_Process_Id, _Reason} -> 
            io:format("[clienteInfo] Un proceso ha fallado, el cliente se cerrara.~nSi desea volvere a iniciar el cliente utilice la funcion starCli().~n"),
            unregister(clienteInfo),
            cerrarSocket(Socket),   
            exit(abnormal)          
    end.


%
%% send_tcpInit: Inicilializa el proceso de send_tcp
%
send_tcpInit(Socket) ->
    process_flag(trap_exit, true),
    link(whereis(clienteInfo)),
    send_tcp(Socket).


%
%%send_tcp : Se encarga de mandar las requests al servidor
%
send_tcp(Socket)->

    receive
        {append, R, N} ->

            Mensaje = {append, R, N},
            Msg     = term_to_binary(Mensaje),
            trySend(Socket, Msg);
            
        {get, N} ->

            Mensaje = {get,N},
            Msg     = term_to_binary(Mensaje),
            trySend(Socket, Msg);
        

        {fin, PId} -> 
            
            cerrarSocket(Socket),
            unregister(send_tcp),
            PId ! {sendFinOk};

        {'EXIT', _Exiting_Process_Id, _Reason} -> 

            io:format("[send_tcp] Un proceso ha fallado, el cliente se desconecto.~nSi desea volver a iniciar el cliente utilice la funcion starCli().~n"),
            unregister(send_tcp),
            cerrarSocket(Socket),
            exit(abnormal)            
    
      
    end.


%
%%receive_tcpInit : Inicializa el proceso de receive_tcp.
%
receive_tcpInit(Socket)->
    process_flag(trap_exit, true),
    link(whereis(send_tcp)),
    link(whereis(clienteInfo)),
    receive_tcp(Socket).

%
%% receive_tcp : Se encarga de recibir las respuestas del servidor
%
receive_tcp(Socket) ->

    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
            
        {ok, Paquete} ->
            Mensaje = binary_to_term(Paquete),
            io:format("El mensaje recibido fue: ~p~n",[Mensaje]),
            receive_tcp(Socket);

        {error, timeout} -> 
            receive
                {fin, PId} ->
                    cerrarSocket(Socket),       
                    unregister(receive_tcp),
                    PId ! {receiveFinOk};
                
                {'EXIT', _Exiting_Process_Id, _Reason} -> 
                    io:format("[receive_tcp] Un proceso ha fallado, el cliente se desconecto.~nSi desea volvere a iniciar el cliente utilice la funcion starCli().~n"),
                    unregister(receive_tcp),
                    cerrarSocket(Socket),
                    exit(abnormal)   
            after 
                0 -> receive_tcp(Socket)
            end;

        
        {error, _Reason} -> 
            io:format("[receive_tcp] Hubo un error con la conexion, el cliente se va a cerrar ~nSi desea volvere a iniciar el cliente utilice la funcion starCli().~n"),
            cerrarSocket(Socket),
            unregister(receive_tcp),
            exit(abnormal)

                    
    end.

%
%%lGet : Se encarga de hacer el request para obtener la ultima secuencia del servidor 
%
lGet()->
    clienteInfo ! {opRequest, self()},
    receive
        {opAck, N} -> send_tcp ! {get, N}
    end,
    ok.
    
%
%% lAppend : Se encarga de hacer el request para agregar un record a la secuencia del servidor
%
lAppend(R)->

    
    clienteInfo ! {opRequest, self()}, 
    receive
        {opAck, N} -> send_tcp ! {append, R, N}
    end,
    ok.


%
%% cerrarSocket : Intenta cerrar el socket
%
cerrarSocket(Socket) ->

    case catch(gen_tcp:close(Socket)) of
        ok   -> ok;

        _Any -> shaTabaChe    
    end,
    ok.    


%
%% trySend : Intenta enviar las request al server. En caso de no ser posiible sale 
%            de forma anormal
trySend(Socket, Msg) ->

    case catch(gen_tcp:send(Socket, Msg)) of

        ok -> send_tcp(Socket);

        _Any -> unregister(send_tcp),
                exit(abnormal)
    end.

%
%% intentarFinalizar: Encargado de mandar las señales de fin a los procesos
%                     
intentarFinalizar(Objetivo, Respuesta) ->

    Id = self(),
    case catch (Objetivo ! {fin, Id}) of

        {fin, Id} -> 
            receive 
                Respuesta -> ok
            after
                5000 -> ok 
            end;

        _Any -> ok
    end.

%
%% stopCli : Termina con la ejecucion del cliente
%
stopCli()->
    
    io:format("Cerrando el cliente ~n"),
    intentarFinalizar(receive_tcp, {receiveFinOk}),
    intentarFinalizar(clienteInfo, {contFinOk}),
    intentarFinalizar(send_tcp   , {sendFinOk}),
    io:format("Cliente cerrado exitosamente ~n"),
    init:stop(),
    chau.
    


