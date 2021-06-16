-module(ledgerSrv).

%%Librerias del Ledger distribuido
-export([ledgerInit/0,ledger/3]).

%%Librerias de control de la conexion tcp
-export([socketHandler/0,tcp_handler/2]).

%%Libreria de acceso
-export([entryPointInit/0]).

%%Librerias importadas del broadcast atomico para el 
%%funcionamiento del ledger
-import(broadcastAtomico,[start/0,aBroadcast/1]).

%%Puerto de conexion tcp
-define(Puerto, 1248).

%
%%ledgerInit:Inicializa el boradcast atomico y los actores principales de
%           el ledger.
ledgerInit() ->

    start(),
     
    register(ledgersrv, spawn(?MODULE, ledger,[[],[],[]])),
    
    ok.

%
%%entryPointInit: Se inicia el entryPoint al server.
%                Inicia con 1 ya que el contador debe empezar en 1
entryPointInit() ->
     case gen_tcp:listen(?Puerto, [binary, {active, false}]) of

        {ok, ListenSocket} -> 
            register(socketHandler, spawn(?MODULE, socketHandler,[])),
            entryPoint(1,ListenSocket);
    
        {error, Reason} -> 
            io:format("Falló escuchar el puerto por: ~p~n",[Reason])

    end.        
%
%%entryPoint: Escucha el puerto, encargado de recibir a nuevos
%             clientes y asignarlos a algun nodo
entryPoint(IndiceSelector,ListenSocket) ->

            case gen_tcp:accept(ListenSocket) of

                {ok, Socket} -> 

                    io:format("Estos son los nodos de la red : ~p~n",[nodes(connected)]),
                    case catch lists:nth(IndiceSelector,nodes(connected)) of

                        {'EXIT', _Reason} -> Node = lists:nth(1,nodes(connected)),
                                            spawn(?MODULE, tcp_handler,[Socket,Node]),
                                            entryPoint(2,ListenSocket);

                        Value             -> Node = Value,
                                            spawn(?MODULE, tcp_handler,[Socket,Node]),
                                            entryPoint(IndiceSelector+1,ListenSocket)

                    end
            
            end.    
            


%
%%socketHandler: Se encarga de recibir los socket de los
%                clientes conectados y crear un actor (tcp_handler) por cada uno
%                que escucha el socket del cliente.
socketHandler() ->

    receive

        {msgToSend, Msj ,Socket} ->
            io:format("Se envio el mensaje del socket ~p con el mensaje ~p~n",[Socket,Msj]),
            gen_tcp:send(Socket,Msj),
            socketHandler();

        Any -> io:format("Sino se envio ~p~n",[Any])

    end.


%
%%tcp_handler: Se encarga de recibir los mensajes de un cliente 
%              a travez de un socket
tcp_handler(Socket, Node)->
    case gen_tcp:recv(Socket, 0) of
    
        {ok, Paquete} ->
            
            Mensaje = binary_to_term(Paquete),
            case Mensaje of

                {get, C}     -> {ledgersrv, Node} ! {get, C, Socket},
                                tcp_handler(Socket,Node);
                            
                {append,R,C} -> {ledgersrv, Node} ! {append, R, C, Socket},
                                tcp_handler(Socket,Node)
                            
            end;  
       
        {error, closed} ->
           io:format("El cliente cerró la conexión~n")

    end.

%
%%ledger: Se encarga de recibir y contestar las peteciones
%         de los clientes
ledger(S_i,StackGet,StackAppend)->

    receive
        {get, C, Socket} ->
            
            aBroadcast({get, C, Socket}),
            NewStackGet = StackGet++[{Socket,C}],
            ledger(S_i, NewStackGet, StackAppend); 

        {append,R,C,Socket}->

            aBroadcast({append,R,C,Socket}),
            NewStackAppend = StackAppend++[{C,R}],
            ledger(S_i, StackGet,NewStackAppend);

        {deliver, Response} ->

            case Response of      
                {get, C, Socket} ->

                    Belong = lists:any(fun(X) -> X == {Socket,C} end, StackGet),
                    io:format("Se comparo el elemento ~p en la lista ~p ~n",[{Socket,C},StackGet]),
                    if  Belong -> 
                        io:format("Se devuelve ~p a ~p ~n",[Response, Socket]),
                        TermToSend = term_to_binary({get, C, S_i}),
                        [TCP] = nodes(hidden),
                        {socketHandler, TCP} ! {msgToSend, TermToSend ,Socket},
                
                        NewStackGet = lists:delete({Socket,C},StackGet),
                        ledger(S_i, NewStackGet,StackAppend);
                    
                        true -> 
                            ledger(S_i, StackGet,StackAppend)
                    end;

                {append,R,C,Socket} ->
                    
                    Belong = lists:any(fun(X) -> X == R end,  S_i),
                    io:format("Se comparo el elemento ~p en la lista ~p ~n",[R,S_i]),
                    if  Belong == false ->
                         io:format("Se devuelve ~p a ~p ~n",[Response, Socket]),
                        NewS_i = S_i ++ [R],

                        Belong2 = lists:any(fun(X) -> X == {R,C} end,  StackAppend),
                
                        if Belong2 -> 
                                io:format("Se devuelve ~p a ~p ~n",[Response, Socket]),   
                                TermToSend = term_to_binary({appendRes,ack,C}),

                                [TCP] = nodes(hidden),
                                {socketHandler, TCP} ! {msgToSend, TermToSend ,Socket},
                                
                                NewStackAppend = lists:delete({C,R},StackAppend),
                                ledger(NewS_i, StackGet,NewStackAppend);        

                            true -> ledger(NewS_i, StackGet,StackAppend)
                        end; 
                       
                        true -> ledger(S_i, StackGet,StackAppend)
                    end
            end

    end.
    