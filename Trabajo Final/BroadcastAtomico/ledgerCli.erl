-module(ledgerCli).

-export([startCli/0,lGet/0,lAppend/1]).

-export([tcp_handler/1,contador/1]).

-define(Dir, "localhost").
-define(Puerto, 1234).

startCli()->
    
    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
        {ok, Socket} -> register(tcp_handler  , spawn(?MODULE,tcp_handler, [Socket])),
                        register(contador     , spawn(?MODULE, contador , [0]));

        {error, _Reason} -> exit
    end,    
    ok.


lGet()->
    contador ! {up,self()},
    receive
        {upAck, N} -> 
            tcp_handler ! {get, N, self()},
            receive
                {ledger, V} -> V  
            end    
    end.
    

lAppend(R)->
    %%verificar tripla
    contador ! {up, self()}, 
    receive
        {upAck,N} -> tcp_handler ! {append, N, R, self()},
                    receive
                        {ledger, Res} -> Res  
                    end    
    end.


contador(N)->
    receive
        {up, PID} -> Num = N + 1,
                     PID ! {upAck,Num},
                     contador(Num)
    end.

tcp_handler(Socket)->

    receive
        {append, N, R, PID} ->

            Mensaje = {append,N,R},
            Msg = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            receive_tcp(Socket,PID);
            
        {get, N, PID} ->

            Mensaje = {get,N},
            Msg = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            receive_tcp(Socket,PID)
              
    end.

receive_tcp(Socket, PID) ->

    case gen_tcp:recv(Socket,0) of
            
                {ok, Paquete} ->
                        Mensaje = binary_to_term(Paquete),       
                        PID ! Mensaje,
                        tcp_handler(Socket);
                    
                {err, Reason }-> io:format("Error: ~p~n",[Reason])
                    
    end.

