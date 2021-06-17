-module(broadcastAtomico).

%%-include("broadcastAtomico.hrl").

%%Funciones de control
-export([start/0,stop/0]).

%%Libreria de acceso
-export([aBroadcast/1]).


-export([aDeliverInit/0,aSequencer/5,aSenderInit/3]).

%%Funcion para facilitar la conexion de nodos
-export([link_nodos/2]).
%%-define(Nodos,4).

%%c(broadcastAtomico,[{d,bandera}]). para cargar la bandera

%
%% Se puede elegir entre el funcionamiento del broadcastAtomico
%  para usarla indibudialmente o en conjunto con el ledger
-ifdef(bandera).
    -define(enviar(Tripla),ledgersrv ! {deliver, Tripla}).
-else.
    -define(enviar(Mensaje), io:format("Deliver : ~p ~n",[Mensaje])).
-endif.

-record(paqueteSinOrden, {msg,identificador,ordenPropuesto}).

%%start() se encarga de inicializar el nodo
start() ->

    io:format("Se inicia el nodo ~p~n",[node()]),
    CantNodos = contarElementos(nodes())+1,
    register(sequencer, spawn(?MODULE, aSequencer,[dict:new(),0,0,0,0])),
    register(deliver,   spawn(?MODULE, aDeliverInit  ,[])),
    register(sender,    spawn(?MODULE, aSenderInit   ,[0,dict:new(),CantNodos])),
    ok.
%
%% link_nodos: Conecta N nodos con el nombre "nodoN@'nombreDeLaPc'"
%
link_nodos(1,PCName) ->
    net_adm:ping(list_to_atom("nodo1@"++PCName));

link_nodos(N,PCName) ->    
    net_adm:ping(list_to_atom("nodo"++integer_to_list(N)++"@"++PCName)),
    link_nodos(N-1,PCName).

%
%% trySendFin: Intenta mandar fin a un atomo registrado y espera su respuesta
%              si recibe respuesta de fin, se deregistra
trySendFin(Registro, Response) ->
    
    case catch (Registro ! fin) of
        fin -> 
            receive 
                Response -> unregister(Registro)
            end;

        _ErrorCatch -> ok
    end.
     
%%stop() se encarga de matar el nodo
stop() ->
    trySendFin(sequencer, sequencerFinOk),
    trySendFin(deliver, deliverFinOk),
    trySendFin(sender, senderFinOk),
    init:stop().

%
%%aBroadcast: Envia el mensaje como argumento a todos los nodos conectados
%
aBroadcast(Mensaje) ->
    
    Nodos = nodes(),
    if  Nodos /= [] ->

        sender ! {msg, Mensaje},
        ok;

        true -> deliver ! {msg, Mensaje},
                ok
    end.

%
%%actualizarDiccionario : Actualiza el diccionaria de mensajes enviados cuando un nodo de la red
%                         se desconecta
%
actualizarDiccionario([],DicMsgToSend,_Node) ->
    DicMsgToSend;
actualizarDiccionario([Key|Keys],DicMsgToSend,Node) ->

    {PropToReceive,ListaPropuestas,ListaNodos} = dict:fetch(Key,DicMsgToSend),

    Prep = lists:member(Node,ListaNodos),
    
    if  Prep -> 
                NewListaNodos = lists:delete(Node,ListaNodos),
                Contador = PropToReceive - 1,
                
                if Contador == 0 ->  broadcast(ListaPropuestas, Key),
                                     NewDic = dict:erase(Key,DicMsgToSend),
                                     actualizarDiccionario(Keys,NewDic,Node);

                   true          ->  NewDic = dict:store(Key,{Contador, ListaPropuestas, NewListaNodos},DicMsgToSend),
                                     actualizarDiccionario(Keys,NewDic,Node)

                           
                end;

        true -> actualizarDiccionario(Keys,DicMsgToSend,Node)
    end.

%
%% aSenderInit: Inicializa el proceso sender
%
aSenderInit(CantMensajesEnviados,DicMsgToSend,CantNodos)->
    link(whereis(sequencer)),
    link(whereis(deliver)),
    Nodes = nodes(),
    lists:foreach(fun(X) -> monitor_node(X,true) end, Nodes),

    aSender(CantMensajesEnviados,DicMsgToSend,CantNodos).

%
%% aSender: recibe el mensaje, envia la propuesta, espera las propuestas
%           elige el mayor numero propuesto y envia la seleccion a todos los nodos
%
aSender(CantMensajesEnviados, DicMsgToSend, CantNodos) ->
    
    receive 

        {'EXIT', _Exiting_Process_Id, _Reason} ->
            %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (No se recibio respuesta del sequencer del mismo nodo)
                        stop();

        {fin, From} -> 
                       From ! senderFinOk,
                       exit(normal);
        
        {nodedown, Node} ->

            %verificar si el nodo tiene que mandar propuesta a uno de los mensajes a enviar
            %En el caso de que este, se quita de la cola de nodos a esperar y se resta en 1
            %la cantidad de nodos que se esta esperando. Si no esta, no se modifica el Diccionario
            NewCantNodos = contarElementos(nodes())+1,
            io:format("El porcentaje de nodos en funcionamiento es ~p~n",[NewCantNodos*100/CantNodos]),
            
            if NewCantNodos*100/CantNodos >= 75 ->

                    io:format("[sender][nodedown] Diccionario sin modificar: ~p~n",[dict:to_list(DicMsgToSend)]),    
                    KeyListMsgToSend = dict:fetch_keys(DicMsgToSend),
                    NewDicMsgToSend  = actualizarDiccionario(KeyListMsgToSend,DicMsgToSend,Node),
                    io:format("[sender][nodedown] Diccionario modificado: ~p~n",[dict:to_list(NewDicMsgToSend)]),    
                    aSender(CantMensajesEnviados,NewDicMsgToSend,CantNodos);

                true -> io:format("[sender][nodedown] Nos vimos en disney~n"),    
                        init:stop()
            end;
           

        {msg, Mensaje} -> 

            %%io:format("[sender] Se inicia el protocolo de envio de >~p<~n",[Mensaje]),

            sequencer ! {crearPaqueteSO, Mensaje, CantMensajesEnviados + 1},

            receive

                {paqueteSO,PaqueteSO} ->
                        
                        %%io:format("[sender] Recibimos el paqueteSinOrden ~p~n",[PaqueteSO]),
                        
                        Nodes = nodes(),

                        CantPropuestasARecebir = contarElementos(Nodes),

                        %%io:format("[sender] Diccionario sin modificar: ~p~n",[dict:to_list(DicMsgToSend)]),

                        NewDic = dict:store(PaqueteSO#paqueteSinOrden.identificador
                                , {CantPropuestasARecebir,[PaqueteSO#paqueteSinOrden.ordenPropuesto + 1],Nodes} %% se inyecta la propuesta del nodo emisor directamente. (por eso el +1)
                                , DicMsgToSend),

                        %%io:format("[sender] Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),

                        lists:foreach(fun(X) -> {sequencer , X } ! {askingForOrder,PaqueteSO} end,Nodes),
                        
                        aSender(CantMensajesEnviados+1,NewDic,CantNodos);

                {'EXIT', _Exiting_Process_Id, _Reason} ->
                        stop()

            after
                5000 ->
                        %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (No se recibio respuesta del sequencer del mismo nodo)
                        stop(),
                        exit(normal)

    
            end;

        {propuestaOrden, PropuestaOrden, IdentificadorMensaje, From} ->

            %%io:format("[sender] Se recibio la propuesta de orden >~p< para el identificador >~p<~n",[PropuestaOrden,IdentificadorMensaje]),

            case dict:find(IdentificadorMensaje, DicMsgToSend) of 
                
                {ok, {CantPropuestasARecebir,ListaPropuestas,Nodos}} ->

                    %%io:format("[sender]  El identificador >~p< esta asociado al mensaje ~p con la propuesta ~p ~n",[IdentificadorMensaje,PropuestaOrden]),

                    
                    %% Actualizar Datos
                    PropToReceive = CantPropuestasARecebir - 1,
                    ListaPropuestasPrima = ListaPropuestas ++ [PropuestaOrden],
                    ListaNuevaNodos = lists:delete(From,Nodos),
                    
                    if 
                        (PropToReceive == 0) ->
                                broadcast(ListaPropuestasPrima, IdentificadorMensaje),
                                NewDic = dict:erase(IdentificadorMensaje, DicMsgToSend),
                                aSender(CantMensajesEnviados,NewDic,CantNodos);   
                
                        true->
                            %%io:format("[sender][sumandoPropuestas] Diccionario sin modificado: ~p~n",[dict:to_list(DicMsgToSend)]),
                            NewDic = dict:store(IdentificadorMensaje
                                    , {PropToReceive,ListaPropuestasPrima,ListaNuevaNodos}
                                    , DicMsgToSend),
                            %%io:format("[sender][sumandoPropuestas] Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                            aSender(CantMensajesEnviados,NewDic,CantNodos)        
                    end;
                    
                error ->

                    %Estado inconsistente del sistema, MUERTE AL TRAIDOR
                    stop(),
                    exit(normal)
            end;
        _Error ->
            %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (se recibio un mensaje re wadafak)
            init:stop()

    end.
%
%% aSequencer: crea el paquete 
%
aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual, TO)-> 
    receive
        {crearPaqueteSO, Mensaje, CantMensajesEnviados} -> 

            PaqueteSO = #paqueteSinOrden{msg = Mensaje
                        , identificador = {node(),CantMensajesEnviados}
                        , ordenPropuesto = OrdenMaximoPropuesto},


            sender ! {paqueteSO,PaqueteSO},
                        
            %%io:format("[sequencer][Local]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),

            NewDic    = dict:store(PaqueteSO#paqueteSinOrden.identificador
                        , {PaqueteSO#paqueteSinOrden.msg,PaqueteSO#paqueteSinOrden.ordenPropuesto}
                        , DicMensajes),
            %%io:format("[sequencer][Local] Diccionario  modificado: ~p~n",[dict:to_list(NewDic)]),
            aSequencer(NewDic,OrdenMaximoAcordado,OrdenMaximoPropuesto + 1, OrdenActual, TO);
    

        {askingForOrder, Paquete} when is_record(Paquete,paqueteSinOrden) -> 
            
            PropuestaOrden = lists:max([OrdenMaximoPropuesto,Paquete#paqueteSinOrden.ordenPropuesto]) + 1,

            {Nodo,_} = Paquete#paqueteSinOrden.identificador,            
            {sender, Nodo} ! {propuestaOrden, PropuestaOrden,Paquete#paqueteSinOrden.identificador,node()},
            
            %%io:format("[sequencer][askingForOrder]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),
            NewDic = dict:store(Paquete#paqueteSinOrden.identificador
                        ,{Paquete#paqueteSinOrden.msg, PropuestaOrden}
                        , DicMensajes),
            %%io:format("[sequencer][askingForOrder]Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
         
            aSequencer(NewDic, OrdenMaximoAcordado, PropuestaOrden, OrdenActual, TO);


        {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada} ->

                NewOrdenMaxAcor = lists:max([PropuestaAcordada,OrdenMaximoAcordado]),
                case dict:find(IdentificadorMensaje, DicMensajes) of
                
                {ok, {Mensaje,_OrdenPropuesto}} ->
                    %%io:format("[sequencer][propuestaAcordada]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),
                    AuxDic = dict:erase(IdentificadorMensaje, DicMensajes),
                    NewDic = dict:store(PropuestaAcordada
                                    , Mensaje
                                    , AuxDic),
                    %%io:format("[sequencer][propuestaAcordada]Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                    aSequencer(NewDic,NewOrdenMaxAcor,OrdenMaximoPropuesto, OrdenActual, 0);
                   
                error ->
                   %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (Mensaje no encontrado en el diccionario)
                    stop(),
                    exit(normal)
                end;

        {fin, From} -> 
            From ! sequencerFinOk,
            exit(normal);
        
        {'EXIT', _Exiting_Process_Id, _Reason} ->
            stop();
        
        _Error ->
            %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (se recibio un mensaje inesperado)
            init:stop()

    after 
        %% Tratamos de mandar a Deliver
        TO -> 
            case dict:find(OrdenActual+1, DicMensajes) of
                {ok, Msg} ->
                            deliver ! {msg, Msg},
                            %%io:format("[sequencer][TO]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),
                            NewDic = dict:erase(OrdenActual+1, DicMensajes),
                            %%io:format("[sequencer][TO]Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                            aSequencer(NewDic,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual + 1, 0);
        
                error -> aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual,infinity)
            end     

    end.


%
%% aDeliver: 
%
aDeliverInit()->
    link(whereis(sequencer)),
    aDeliver().
%
%%aDeliver: 
%
aDeliver() ->
    receive
        {fin, From} -> 
            From ! deliverFinOk,
            exit(normal);
        {msg,M} ->
            ?enviar(M),
            aDeliver();
        
        {'EXIT', _Exiting_Process_Id, _Reason} ->
            stop();

        _Error ->
        %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (se recibio un mensaje inesperado)
            init:stop()
        
        
    end.

%
%%contarElementos cuenta la cantidad de elementos de un lista
%
contarElementos([]) ->
    0;  
contarElementos([_Hd|Tl])->
    1 + contarElementos(Tl).

broadcast(ListaPropuestas,IdentificadorMensaje) ->

    PropuestaAcordada = lists:max(ListaPropuestas),
    lists:foreach(fun(X) -> {sequencer , X } ! {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada} end,nodes()),
    sequencer ! {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada}.
    


    