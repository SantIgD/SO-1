-module(ledgerCli).


%%Funciones de control
-export([startCli/0,stopCli/0]).

%%Libreria de acceso
-export([lGet/0,lAppend/1]).

%%Funciones del cliente del ledger
-export([send_tcp/1,contador/1,receive_tcp/1]).

%%Direccion ip del server
-define(Dir, "localhost").

%%Puerto del server
-define(Puerto, 1248).

-define(TIMEOUT, 1000).

%
%% stopCli : Termina con la ejecucion del cliente
%
stopCli()->
    
    Id = self(),
    case catch (contador ! {fin, Id}) of

        {fin, Id} -> 
            receive 
                {contFinOk} -> unregister(contador)
            end;

        _Any -> ok
    end,
    
    case catch (send_tcp ! {fin, Id}) of
        
        {fin, Id} -> 
            receive 
                {sendFinOk} -> unregister(send_tcp)
            end;
        _Any2 -> ok
    end,

    case  catch(receive_tcp ! {fin, Id}) of

       {fin, Id} ->
            receive 
                {receiveFinOk, Socket} -> unregister(receive_tcp),
                                          gen_tcp:close(Socket)
            end;
        _Any3 -> ok
    end,
    init:stop(),
    chau.
    

%
%%startCli : Se conecta con el servidor y spawnea/registra los procesos 
%            que hacen funcionar el cliente
startCli()->
    
    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
        {ok, Socket} -> register(send_tcp     , spawn(?MODULE, send_tcp   , [Socket])),
                        register(contador     , spawn(?MODULE, contador   , [0])),
                        register(receive_tcp  , spawn(?MODULE, receive_tcp, [Socket]));

        {error, Reason} -> 
            io:format("No se conecto: ~p",[Reason]),
            exit
    end,    
    ok.

%
%%lGet : Se encarga de hacer el request para obtener la ultima secuencia del servidor 
%
lGet()->
    contador ! {opRequest, self()},
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
    contador ! {opRequest, self()}, 
    receive
        {opAck, N} -> send_tcp ! {append, R, N}
    end,
    ok.

%
%%contador : Se encarga de contar la cantidad de requests hechos
%
contador(N)->
    receive
        {opRequest, PID} -> Num = N + 1,
                     PID ! {opAck,Num},
                     contador(Num);

        {fin, PId} -> PId ! {contFinOk}            
    end.

%
%%send_tcp : Se encarga de mandar las requests al servidor
%
send_tcp(Socket)->

    receive
        {append, R, N} ->

            Mensaje = {append, R, N},
            Msg     = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            send_tcp(Socket);
            
        {get, N} ->

            Mensaje = {get,N},
            Msg     = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            send_tcp(Socket);

        {fin, PId} -> PId ! {sendFinOk} 
      
    end.


%
%% receive_tcp : Se encarga de recibir las respuestas del servidor
%
receive_tcp(Socket) ->

    %% los mensajes se encolaban???????
    case gen_tcp:recv(Socket,0,?TIMEOUT) of
            
        {ok, Paquete} ->
            Mensaje = binary_to_term(Paquete), 
            io:format("El mensaje recibido fue: ~p~n",[Mensaje]),
            receive_tcp(Socket);

        {error, timeout} -> 
            receive
                {fin, PId} -> PId ! {receiveFinOk} 
            after 
                0 -> receive_tcp(Socket)
            end;

        
        {error, Reason} -> 
            io:format("Error: ~p~n",[Reason]),
            stopCli(),
            unregister(receive_tcp)

                    
    end.

