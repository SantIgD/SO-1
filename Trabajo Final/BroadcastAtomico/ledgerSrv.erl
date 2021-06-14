-module(ledgerSrv).

-export([ledgerInit/0]).
-import(broadcastAtomico,[start/0,aBroadcast/1]).

ledgerInit() ->
    start(),
    register(ledgersrv,self()),
    ledger([],[],[]),

    ok.

ledger(S_i,StackGet,StackAppend)->

    receive

        {C,get} ->
            
            aBroadcast({get,self(),C}),
            NewStackGet = StackGet++[{self(),C}],
            ledger(S_i, NewStackGet,StackAppend);

        {deliver, Response} ->

            case Response of      
                {get,PID,C} ->

                    Belong = lists:any(fun(X) -> X == {PID,C} end, StackGet),
                    if  Belong -> 

                        %MANDAR AL CLIENTE {getRes,C,S_i},
                        io:format("El conjunto es  = ,~p~n",[S_i]),
                        NewStackGet = lists:delete({PID,C},StackGet),
                        ledger(S_i, NewStackGet,StackAppend);
                    
                        true -> 
                            ledger(S_i, StackGet,StackAppend)
                    end;

                {append,R,C} ->
                    
                    Belong = lists:any(fun(X) -> X == {R,C} end,  S_i),
                    
                    if  Belong == false ->
                        
                        NewS_i = S_i ++ [R],

                        Belong2 = lists:any(fun(X) -> X == {R,C} end,  StackAppend),
                
                        if Belong2 -> 
                                
                            %% MANDAR AL CLIENTE {appendRes,ACK,C},
                            io:format("Se recibio el ACK de la tripla = ~p~n",[R]),
                            NewStackAppend = lists:delete({R,C},StackAppend),
                            ledger(NewS_i, StackGet,NewStackAppend);        

                            true -> ledger(NewS_i, StackGet,StackAppend)
                        end;    
                        true -> ledger(S_i, StackGet,StackAppend)
                    end 
            end;     
        {C,append,R}->
            aBroadcast({append,R,C}),
            NewStackAppend = StackAppend++[{C,R}],
            ledger(S_i, StackGet,NewStackAppend)

    end.
    