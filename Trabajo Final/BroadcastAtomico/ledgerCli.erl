-module(ledgerCli).

-export([startCli/0,lGet/0,lAppend/1,receive_tcp/1]).

-export([send_tcp/1,contador/1]).

-define(Dir, "localhost").
-define(Puerto, 1239).

startCli()->
    
    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
        {ok, Socket} -> register(send_tcp     , spawn(?MODULE,send_tcp, [Socket])),
                        register(contador     , spawn(?MODULE, contador , [0])),
                        register(receive_tcp  , spawn(?MODULE,receive_tcp, [Socket]));

        {error, _Reason} -> exit
    end,    
    ok.


lGet()->
    contador ! {up,self()},
    receive
        {upAck, N} -> 
            send_tcp ! {get, N, self()},
            receive
                {get, _C, S_i} -> S_i  
            end    
    end.
    

lAppend(R)->
    %%verificar tripla
    contador ! {up, self()}, 
    receive
        {upAck,N} -> send_tcp ! {append, R, N, self()},
                    receive
                        {appendRes,ack,_Mensaje }-> ack
                    end    
    end.


contador(N)->
    receive
        {up, PID} -> Num = N + 1,
                     PID ! {upAck,Num},
                     contador(Num)
    end.

send_tcp(Socket)->

    receive
        {append, R, N, PID} ->

            Mensaje = {append,N,R},
            Msg     = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            receive
                {receive_tcp , Msj} -> PID ! {appendRes, ack, Msj}
            end;    
            
        {get, N, PID} ->

            Mensaje = {get,N},
            Msg = term_to_binary(Mensaje),
            gen_tcp:send(Socket, Msg),
            receive
                {receive_tcp , Msj} -> 
                    {get, Cont, S_i} = Msj,
                    PID ! {get, Cont, S_i}
            end
              
    end.

receive_tcp(Socket) ->

    case gen_tcp:recv(Socket,0) of
            
                {ok, Paquete} ->
                        Mensaje = binary_to_term(Paquete),       
                        send_tcp ! {receive_tcp, Mensaje},
                        receive_tcp(Socket);
                    
                {error, Reason} -> 
                        io:format("Error: ~p~n",[Reason])
                    
    end.

