-module(ledgerSrv).

-export([ledgerInit/0,ledger/4,tcp_handler/1]).
-import(broadcastAtomico,[start/0,aBroadcast/1]).
-define(Puerto, 1234).

ledgerInit() ->
    start(),
    case gen_tcp:listen(?Puerto, [binary, {active, false}])of

        {ok, ListenSocket} ->  
            case gen_tcp:accept(ListenSocket) of

                {ok, Socket} -> 
                    spawn(?MODULE, tcp_handler,[Socket]), 
                    register(ledgersrv,spawn(?MODULE, ledger,[[],[],[],Socket]));
    
                {error, Reason} ->
                    io:format("Falló al intentar aceptar un socket por: ~p~n",[Reason])

             end;    
            

        {error, Reason} -> 
            io:format("Falló escuchar el puerto por: ~p~n",[Reason])

    end,
    ok.

tcp_handler(Socket)->
    case gen_tcp:recv(Socket, 0) of
    
        {ok, Paquete} ->
            
            
            Mensaje = binary_to_term(Paquete),
            case Mensaje of

                {get, C}     -> ledger ! {get, C},
                                tcp_handler(Socket);
                            
                {append,R,C} -> ledger ! {append,R,C},
                                tcp_handler(Socket)
                            
            end;  
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")

    end.


ledger(S_i,StackGet,StackAppend,Socket)->

    receive

        {get,C} ->
            
            aBroadcast({get, C, self()}),
            NewStackGet = StackGet++[{self(),C}],
            ledger(S_i, NewStackGet, StackAppend, Socket);

        {deliver, Response} ->

            case Response of      
                {get, C, PID} ->

                    Belong = lists:any(fun(X) -> X == {PID,C} end, StackGet),
                    if  Belong -> 

                        Response = term_to_binary({get, C, PID}),  
                        gen_tcp:send(Socket, Response),
                        io:format("El conjunto es  = ,~p~n",[S_i]),
                        NewStackGet = lists:delete({PID,C},StackGet),
                        ledger(S_i, NewStackGet,StackAppend,Socket);
                    
                        true -> 
                            ledger(S_i, StackGet,StackAppend,Socket)
                    end;

                {append,R,C} ->
                    
                    Belong = lists:any(fun(X) -> X == {R,C} end,  S_i),
                    
                    if  Belong == false ->
                        
                        NewS_i = S_i ++ [R],

                        Belong2 = lists:any(fun(X) -> X == {R,C} end,  StackAppend),
                
                        if Belong2 -> 
                                
                            Response = term_to_binary({appendRes,ack,C}),
                            %%io:format("Se recibio el ACK de la tripla = ~p~n",[R]),
                            gen_tcp:send(Socket, Response),
                            NewStackAppend = lists:delete({R,C},StackAppend),
                            ledger(NewS_i, StackGet,NewStackAppend,Socket);        

                            true -> ledger(NewS_i, StackGet,StackAppend,Socket)
                        end; 
                       
                        true -> ledger(S_i, StackGet,StackAppend,Socket)
                    end
            end;     
        {append,R,C}->
            aBroadcast({append,R,C}),
            NewStackAppend = StackAppend++[{C,R}],
            ledger(S_i, StackGet,NewStackAppend,Socket)

    end.
    