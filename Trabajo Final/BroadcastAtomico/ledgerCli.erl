-module(ledgerCli).

-export([startCli/0,lGet/0,lAppend/1,receive_tcp/1]).

-export([send_tcp/1,contador/1]).

-define(Dir, "localhost").
-define(Puerto, 1248).

startCli()->
    
    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
        {ok, Socket} -> register(send_tcp     , spawn(?MODULE,send_tcp, [Socket])),
                        register(contador     , spawn(?MODULE, contador , [0])),
                        register(receive_tcp  , spawn(?MODULE,receive_tcp, [Socket]));

        {error, Reason} -> 
            io:format("No se conecto: ~p",[Reason]),
            exit
    end,    
    ok.


lGet()->
    contador ! {up,self()},
    receive
        {upAck, N} -> 
            send_tcp ! {get, N}
    end,
    ok.
    

lAppend(R)->
    %%verificar tripla
    contador ! {up, self()}, 
    receive
        {upAck,N} -> send_tcp ! {append, R, N}
    end,
    ok.


contador(N)->
    receive
        {up, PID} -> Num = N + 1,
                     PID ! {upAck,Num},
                     contador(Num)
    end.

send_tcp(Socket)->

    receive
        {append, R, N} ->

            Mensaje = {append, R, N},
            Msg     = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            send_tcp(Socket);
            
        {get, N} ->

            Mensaje = {get,N},
            Msg = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            send_tcp(Socket)
              
    end.

receive_tcp(Socket) ->

    case gen_tcp:recv(Socket,0) of
            
                {ok, Paquete} ->
                        Mensaje = binary_to_term(Paquete), 
                              
                        io:format("El mensaje recibido fue: ~p~n",[Mensaje]),
                        receive_tcp(Socket);
                    
                {error, Reason} -> 
                        io:format("Error: ~p~n",[Reason])
                    
    end.

