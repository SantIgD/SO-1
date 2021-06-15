-module(ledgerSrv).

-export([ledgerInit/0,ledger/3,tcp_handler/1,entryPointInit/0,socketHandler/0]).
-import(broadcastAtomico,[start/0,aBroadcast/1]).
-define(Puerto, 1234).

entryPointInit() ->

    entryPoint(1).

entryPoint(IndiceSelector) ->

    case gen_tcp:listen(?Puerto, [binary, {active, false}])of

        {ok, ListenSocket} -> 

            case gen_tcp:accept(ListenSocket) of

                {ok, Socket} -> 
                    
                    case catch lists:nth(IndiceSelector,nodes()) of

                        {'EXIT', _Reason} -> Node = lists:nth(1,nodes()),
                                            {sockethandler, Node} ! {socketCliente, Socket},
                                            entryPoint(2);

                        Value             -> Node = Value,
                                            {sockethandler, Node} ! {socketCliente, Socket},
                                            entryPoint(IndiceSelector+1)

                    end;
            
                {error, Reason} ->
                    io:format("Falló al intentar aceptar un socket por: ~p~n",[Reason])

             end;    
            

        {error, Reason} -> 
            io:format("Falló escuchar el puerto por: ~p~n",[Reason])

    end,
    ok.

socketHandler() ->

    receive

        {socketCliente, Socket} ->
            spawn(?MODULE, tcp_handler,[Socket]),
            socketHandler()

    end,

    ok.

ledgerInit() ->
    start(),
    register(sockethandler, spawn(?MODULE, socketHandler,[])), 
    register(ledgersrv    , spawn(?MODULE, ledger,[[],[],[]])),
    ok.


tcp_handler(Socket)->
    case gen_tcp:recv(Socket, 0) of
    
        {ok, Paquete} ->
            
            
            Mensaje = binary_to_term(Paquete),
            case Mensaje of

                {get, C}     -> ledger ! {get, C, Socket},
                                tcp_handler(Socket);
                            
                {append,R,C} -> ledger ! {append,R,C,Socket},
                                tcp_handler(Socket)
                            
            end;  
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")

    end.


ledger(S_i,StackGet,StackAppend)->

    receive

        {get, C, Socket} ->
            
            aBroadcast({get, C, Socket}),
            NewStackGet = StackGet++[{Socket,C}],
            ledger(S_i, NewStackGet, StackAppend);

        {deliver, Response} ->

            case Response of      
                {get, C, Socket} ->

                    Belong = lists:any(fun(X) -> X == {Socket,C} end, StackGet),
                    if  Belong -> 

                        Response = term_to_binary({get, C, S_i}),  
                        gen_tcp:send(Socket, Response),
                        io:format("El conjunto es  = ,~p~n",[S_i]),
                        NewStackGet = lists:delete({Socket,C},StackGet),
                        ledger(S_i, NewStackGet,StackAppend);
                    
                        true -> 
                            ledger(S_i, StackGet,StackAppend)
                    end;

                {append,R,C,Socket} ->
                    
                    Belong = lists:any(fun(X) -> X == {R,C} end,  S_i),
                    
                    if  Belong == false ->
                        
                        NewS_i = S_i ++ [R],

                        Belong2 = lists:any(fun(X) -> X == {R,C} end,  StackAppend),
                
                        if Belong2 -> 
                                
                            Response = term_to_binary({appendRes,ack,C}),
                            %%io:format("Se recibio el ACK de la tripla = ~p~n",[R]),
                            gen_tcp:send(Socket, Response),
                            NewStackAppend = lists:delete({C,R},StackAppend),
                            ledger(NewS_i, StackGet,NewStackAppend);        

                            true -> ledger(NewS_i, StackGet,StackAppend)
                        end; 
                       
                        true -> ledger(S_i, StackGet,StackAppend)
                    end
            end;     
        {append,R,C,Socket}->
            aBroadcast({append,R,C,Socket}),
            NewStackAppend = StackAppend++[{C,R}],
            ledger(S_i, StackGet,NewStackAppend)

    end.
    