-module(ledgerSrv).

%%Funciones del Ledger distribuido
-export([startLedger/0,ledgerInit/3]).

%%Funciones de control de la conexion tcp
-export([socketHandler/0,tcp_handlerInit/2]).

%%Funcion de control
-export([entryPointInit/0]).

%%Librerias importadas del broadcast atomico para el 
%%funcionamiento del ledger
-import(broadcastAtomico, [start/0, stop/0, aBroadcast/1]).

-define(TIMEOUT, 1000).

%%Puerto de conexion tcp
-define(Puerto, 1248).

%
%%startLedger:Inicializa el boradcast atomico y los actores principales de
%           el ledger.
startLedger() ->

    start(),
     
    register(ledgersrv, spawn(?MODULE, ledgerInit,[[],[],[]])),
    
    ok.

entryPointStop()->ok.
%
%%entryPointInit: Se inicia el entryPoint al server.
%                 Inicia con 1 ya que el contador debe empezar en 1
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
entryPoint(IndiceSelector, ListenSocket) ->

    case gen_tcp:accept(ListenSocket, ?TIMEOUT) of

        {ok, Socket} -> 

            Nodes = nodes(connected),
            if 
                Nodes /= [] ->  
                    
                    io:format("Estos son los nodos de la red : ~p~n",[Nodes]),
                    case catch lists:nth(IndiceSelector, Nodes) of

                        {'EXIT', _Reason} -> 
                            Node = lists:nth(1, Nodes),
                            spawn(?MODULE, tcp_handlerInit, [Socket, Node]),
                            entryPoint(2, ListenSocket);
                        

                        Value -> 
                            Node = Value,
                            spawn(?MODULE, tcp_handlerInit, [Socket,Node]),
                            entryPoint(IndiceSelector + 1, ListenSocket)

                    end;

                true -> 
                    cerrarSocket(ListenSocket),       
                    exit(abnormal)
            end;
            
            
        {error, timeout} ->

            Nodes = nodes(connected),
            if 
                Nodes == [] ->  
                    cerrarSocket(ListenSocket),       
                    exit(abnormal);

                true -> entryPoint(IndiceSelector, ListenSocket)
            end;   
                            

        {error, _Reason} -> cerrarSocket(ListenSocket),
                            exit(abnormal)  
    end.    
    


%
%%socketHandler: Se encarga de recibir los socket de los
%                clientes conectados y crear un actor (tcp_handler) por cada uno
%                que escucha el socket del cliente.
socketHandler() ->

    receive

        {msgToSend, Msj, Socket} ->
            io:format("Se envio el mensaje del socket ~p con el mensaje ~p~n",[Socket,Msj]),
            trySend(Socket,Msj),
            socketHandler();
            

        Any -> io:format("Sino se envio ~p~n",[Any])

    end.



tcp_handlerInit(Socket, Node)->

    monitor_node(Node, true),
    tcp_handler(Socket, Node).


%
%%tcp_handler: Se encarga de recibir los mensajes de un cliente 
%              a travez de un socket
tcp_handler(Socket, Node) ->

    case gen_tcp:recv(Socket, 0) of
    
        {ok, Paquete} ->
            
            Mensaje = binary_to_term(Paquete),
            receive 
                {nodedown, Node} -> 
                    Nodes = nodes(connected),
                    if 
                        Nodes /= [] ->
                            NewNode = lists:nth(rand:uniform(contarElementos(Nodes)), Nodes),
                            monitor_node( NewNode, true),
                            deliverMensaje(Mensaje, NewNode, Socket),
                            tcp_handler(Socket, NewNode);

                        true -> cerrarSocket(Socket),
                                exit(abnormal)
                    end;

                Any -> io:format("Recibimos la fruta >~p<, nos morimos ~n",[Any]),
                        cerrarSocket(Socket),
                        exit(abnormal)
            after
                0 -> deliverMensaje(Mensaje, Node, Socket),
                     tcp_handler(Socket, Node)
            end;   
       
        {error, _Reason} ->
            io:format("El cliente cerró la conexión~n"),
            cerrarSocket(Socket),
            exit(abnormal)
    end.



ledgerInit(A,B,C) ->

    link(whereis(sequencer)),
    link(whereis(deliver)),
    link(whereis(sender)),
    ledger(A,B,C).
%
%%ledger: Se encarga de recibir y contestar las peteciones
%         de los clientes
ledger(S_i,StackGet,StackAppend)->

    receive
        {get, C, Socket} ->
            
            aBroadcast({get, C, Socket}),
            NewStackGet = StackGet++[{Socket,C}],
            ledger(S_i, NewStackGet, StackAppend); 

        {append, R, C, Socket}->

            aBroadcast({append, R, C, Socket}),
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

                {append, R, C, Socket} ->
                    
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
            end;

        {'EXIT', _Exiting_Process_Id, _Reason} ->
            unregister(ledger),
            stop()


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

        ok -> ok;

        _Any -> seCayoUnCliente
    end.

contarElementos([]) ->
    0;  
contarElementos([_Hd|Tl])->
    1 + contarElementos(Tl).

deliverMensaje(Mensaje, Node, Socket) ->

    case Mensaje of
        {get, C}       -> {ledgersrv, Node} ! {get, C, Socket};
        {append, R, C} -> {ledgersrv, Node} ! {append, R, C, Socket}           
    end.