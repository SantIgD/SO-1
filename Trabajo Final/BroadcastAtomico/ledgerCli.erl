-module(ledgerCli).


%%Funciones de control
-export([startCli/0,stopCli/0]).

%%Libreria de acceso
-export([lGet/0,lAppend/1]).

%%Funciones del cliente del ledger
-export([send_tcpInit/1,clienteInfo/2,receive_tcpInit/1]).

%%Direccion ip del server
-define(Dir, "localhost").

%%Puerto del server
-define(Puerto, 1248).

-define(TIMEOUT, 1000).


intentarFinalizar(Objetivo, Respuesta) ->

    Id = self(),
    case catch (Objetivo ! {fin, Id}) of

        {fin, Id} -> 
            receive 
                Respuesta -> unregister(Objetivo)
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
    intentarFinalizar(clienteInfo, {contFinOk}),
    intentarFinalizar(send_tcp, {sendFinOk}),
    intentarFinalizar(receive_tcp, {receiveFinOk}),
    io:format("Cliente cerrado exitosamente ~n"),
    init:stop(),
    chau.
    

%
%%startCli : Se conecta con el servidor y spawnea/registra los procesos 
%            que hacen funcionar el cliente
startCli()->
    
    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
        {ok, Socket} ->  
            register(clienteInfo  , spawn(?MODULE, clienteInfo   , [0, Socket])),
            register(send_tcp     , spawn(?MODULE, send_tcpInit   , [Socket])),
            register(receive_tcp  , spawn(?MODULE, receive_tcpInit, [Socket]));

        {error, Reason} -> 
            io:format("No se conecto: ~p",[Reason]),
            exit
    end,    
    ok.

%
%%lGet : Se encarga de hacer el request para obtener la ultima secuencia del servidor 
%
lGet()->
    clienteInfo ! {opRequest, self()},
    receive
        {opAck, N} -> 
            send_tcp ! {get, N}
    end,
    ok.
    
%
%% lAppend : Se encarga de hacer el request para agregar un record a la secuencia del servidor
%
lAppend(R)->

    %%verificar tripla???????
    clienteInfo ! {opRequest, self()}, 
    receive
        {opAck, N} -> send_tcp ! {append, R, N}
    end,
    ok.

%
%%clienteInfo : Se encarga de contar la cantidad de requests hechos
%
clienteInfo(N, Socket)->
    receive
        {opRequest, PID} -> Num = N + 1,
                     PID ! {opAck,Num},
                     clienteInfo(Num, Socket);

        {fin, PId} -> 
            cerrarSocket(Socket),
            PId ! {contFinOk};

        {'EXIT', _Exiting_Process_Id, _Reason} -> unregister(clienteInfo),
                                                  cerrarSocket(Socket),   
                                                  exit(abnormal)          
    end.


send_tcpInit(Socket) ->

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
            PId ! {sendFinOk};

        {'EXIT', _Exiting_Process_Id, _Reason} -> 

            unregister(send_tcp),
            cerrarSocket(Socket),
            exit(abnormal)            
    
      
    end.

receive_tcpInit(Socket)->
    link(whereis(send_tcp)),
    link(whereis(clienteInfo)),
    receive_tcp(Socket).

%
%% receive_tcp : Se encarga de recibir las respuestas del servidor
%
receive_tcp(Socket) ->

    %% los mensajes se encolaban???????
    case gen_tcp:recv(Socket, 0, ?TIMEOUT) of
            
        {ok, Paquete} ->
            Mensaje = binary_to_term(Paquete),
            io:format("El mensaje recibido fue: ~p~n",[Mensaje]),
            receive_tcp(Socket);

        {error, timeout} -> 
            receive
                {fin, PId} -> cerrarSocket(Socket),
                              PId ! {receiveFinOk};
                
                {'EXIT', _Exiting_Process_Id, _Reason} -> unregister(receive_tcp),
                                                          cerrarSocket(Socket),
                                                          exit(abnormal)   
            after 
                0 -> receive_tcp(Socket)
            end;

        
        {error, Reason} -> 
            io:format("Error: ~p~n",[Reason]),
            cerrarSocket(Socket),
            unregister(receive_tcp),
            exit(abnormal)

                    
    end.

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
