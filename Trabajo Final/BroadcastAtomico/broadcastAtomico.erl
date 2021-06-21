-module(broadcastAtomico).

%%-include("broadcastAtomico.hrl").

%%Funciones de control
-export([start/0,stop/0]).

%%Libreria de acceso
-export([aBroadcast/1]).


-export([aDeliverInit/0,aSequencerInit/5,aSenderInit/3]).

%%Funcion para facilitar la conexion de nodos
-export([link_nodos/2]).

%%Porcentaje minimo de nodos prendidos para que el
%%server se mantenga operativo
-define(PORCENTAJE,30).

%%c(broadcastAtomico,[{d,bandera}]). para cargar la bandera



%
%% Se puede elegir entre el funcionamiento del broadcastAtomico
%  para usarla indibudialmente o en conjunto con el ledger
-ifdef(bandera).
    -define(enviar(Tripla),ledgersrv ! {deliver, Tripla}).
-else.
    -define(enviar(Mensaje), io:format("Deliver : ~p ~n",[Mensaje])).
-endif.

-record(paqueteSinOrden, {msg, identificador, ordenPropuesto}).


%%start() se encarga de inicializar el nodo
start() ->

    io:format("Se inicia el servicio de broadcastAtomico en el nodo ~p~n",[node()]),
    Nodos = nodes(),
    register(sequencer, spawn(?MODULE, aSequencerInit, [dict:new(), 0, 0, 0, 0])),
    register(deliver  , spawn(?MODULE, aDeliverInit  , [])),
    register(sender   , spawn(?MODULE, aSenderInit   , [0, dict:new(), Nodos])),
    ok.

%
%% aSequencerInit: Monitorea a todos los nodos e inicializa el sequencer
%
aSequencerInit(DicMensajes, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO) -> 
    process_flag(trap_exit, true),
    lists:foreach(fun (X) -> monitor_node(X, true) end, nodes()),
    aSequencer(DicMensajes, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO, []).

%
%% aSequencer: Se encarga de manejar los mensajes que recibe de otros nodos o de si mismo
%              # Crea la estructura con la cual comenzara el recorrido de un mensaje
%              # Responde la propuesta de numero de secuencia que considera para un mensaje recibido
%              # Recibe el orden acordado del mensaje y lo encola para mandarselo al deliver
aSequencer(DicMensajes, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO, NumerosPerdidos)-> 
    receive
        {crearPaqueteSO, Mensaje, CantMensajesEnviados} -> 

            PaqueteSO = #paqueteSinOrden{msg = Mensaje
                        , identificador  = {node(), CantMensajesEnviados}
                        , ordenPropuesto = OrdenMaximoPropuesto},

            sender ! {paqueteSO, PaqueteSO},
                        
            

            NewDic = dict:store(PaqueteSO#paqueteSinOrden.identificador
                    , {PaqueteSO#paqueteSinOrden.msg, PaqueteSO#paqueteSinOrden.ordenPropuesto}
                    , DicMensajes),
                    
           
            aSequencer(NewDic, OrdenMaximoAcordado, OrdenMaximoPropuesto+1, OrdenActual, TO, NumerosPerdidos);
    

        {askingForOrder, Paquete} when is_record(Paquete, paqueteSinOrden) -> 
            
            PropuestaOrden = lists:max([OrdenMaximoPropuesto, Paquete#paqueteSinOrden.ordenPropuesto]) + 1,

            {Nodo, _} = Paquete#paqueteSinOrden.identificador,            
            {sender, Nodo} ! {propuestaOrden, PropuestaOrden, Paquete#paqueteSinOrden.identificador, node()},
            
            
            
            NewDic = dict:store(Paquete#paqueteSinOrden.identificador
                        , {Paquete#paqueteSinOrden.msg, PropuestaOrden}
                        , DicMensajes),
           
         
            aSequencer(NewDic, OrdenMaximoAcordado, PropuestaOrden, OrdenActual, TO,NumerosPerdidos);


        {propuestaAcordada, IdentificadorMensaje, PropuestaAcordada} ->

            ListaMayores    = lists:filter(fun (X) -> X < PropuestaAcordada end, NumerosPerdidos),
            Long = contarElementos(ListaMayores),
            ValorAcordado = PropuestaAcordada - Long,
            NewOrdenMaxAcor = lists:max([ValorAcordado, OrdenMaximoAcordado]),

           
            case dict:find(IdentificadorMensaje, DicMensajes) of
            
            {ok, {Mensaje,_OrdenPropuesto}} ->
                
                AuxDic = dict:erase(IdentificadorMensaje, DicMensajes),
                NewDic = dict:store(ValorAcordado
                                , Mensaje
                                , AuxDic),
                
                aSequencer(NewDic, NewOrdenMaxAcor, OrdenMaximoPropuesto, OrdenActual, 0,NumerosPerdidos);
                
            error ->
                %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (Mensaje no encontrado en el diccionario) Antes te deberia haber llegado para que phagas tu propuesta
                init:stop()
            end;
        {nodedown, Node} ->

            
            Keys      = dict:fetch_keys(DicMensajes),
            TupleKeys = lists:filter(fun (X) -> is_tuple(X) end, Keys),
            NodeKey   = lists:filter(fun ({Nodo, _Contador}) -> Node == Nodo end, TupleKeys),
            
            if 
                NodeKey /= [] ->

                    NmsPerdidos = obtenerMensajesPerdidos(NodeKey, DicMensajes),
                    NuevaNumerosPerdidos = NumerosPerdidos ++ NmsPerdidos,
                    OrderKeys = lists:filter(fun (X) -> is_tuple(X) == false end, Keys),
                    MinNumeroMensajePerdido = lists:min(NmsPerdidos),
                    OrderKeysGreaterThanNMP = lists:filter(fun (X) -> X > MinNumeroMensajePerdido end, OrderKeys),
                    

                    if OrderKeysGreaterThanNMP /= [] ->
                    DiccionarioActualizado = reacomodarDiccionario(NmsPerdidos,OrderKeysGreaterThanNMP, DicMensajes, []),
                    aSequencer(DiccionarioActualizado, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO, NuevaNumerosPerdidos);
                    
                    true -> aSequencer(DicMensajes, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO, NuevaNumerosPerdidos)
                    end;
               true -> 
                   aSequencer(DicMensajes, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO, NumerosPerdidos)
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
            OrdenSiguiente = OrdenActual+1, 
            case dict:find( OrdenSiguiente, DicMensajes) of
                {ok, Msg} ->
                    deliver ! {msg, Msg},
                   
                    NewDic = dict:erase(OrdenSiguiente, DicMensajes),
                    
                    aSequencer(NewDic,OrdenMaximoAcordado,OrdenMaximoPropuesto,  OrdenSiguiente, 0,NumerosPerdidos);

                error -> aSequencer(DicMensajes, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, infinity,NumerosPerdidos)
            end     

    end.

%
%% aDeliverInit: Se encarga de inicialiuzar el deliver. 
%
aDeliverInit()->
    process_flag(trap_exit, true),
    link(whereis(sequencer)),
    aDeliver().

%
%%aDeliver: Se encarga de enviar el mensaje en su etapa final
%
aDeliver() ->
    receive

        {msg,M} ->
            ?enviar(M),
            aDeliver();
        
        {fin, From} -> 
            From ! deliverFinOk,
            exit(normal);

        {'EXIT', _Exiting_Process_Id, _Reason} ->
            stop();

        _Error ->
        %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (se recibio un mensaje inesperado)
            init:stop()
    end.

%
%% aSenderInit: Inicializa el proceso sender
%
aSenderInit(CantMensajesEnviados, DicMsgToSend, Nodos)->
    process_flag(trap_exit, true),
    link(whereis(sequencer)),
    link(whereis(deliver)),
    CantNodos = contarElementos(Nodos) + 1,
    lists:foreach(fun(X) -> monitor_node(X, true) end, Nodos),
    aSender(CantMensajesEnviados, DicMsgToSend, CantNodos).

%
%% aSender: Recibe el mensaje a enviar e inicia el protocolo de secuenciacion
%           # Envia el mensaje haciendo request de orden a los demas nodos
%           # Recibe las propuestas de todos los nodos y cuando 
%             ya las recibio para ese mensaje elije el mayor numero
aSender(CantMensajesEnviados, DicMsgToSend, CantNodos) ->
    
    receive 

        {msg, Mensaje} -> 

           
            sequencer ! {crearPaqueteSO, Mensaje, CantMensajesEnviados + 1},
            receive
                {paqueteSO, PaqueteSO} ->
                            
                    
                    
                    Nodes = nodes(),

                    CantPropuestasARecebir = contarElementos(Nodes),

                   
                    
                    % en la lista vacia PaqueteSO#paqueteSinOrden.ordenPropuesto + 1
                    NewDic = dict:store(PaqueteSO#paqueteSinOrden.identificador
                            , {CantPropuestasARecebir, [], Nodes} %% se inyecta la propuesta del nodo emisor directamente. (por eso el +1)
                            , DicMsgToSend),

     
                    lists:foreach(fun(X) -> {sequencer , X} ! {askingForOrder, PaqueteSO} end, Nodes),
                
                    aSender(CantMensajesEnviados+1, NewDic, CantNodos);

                {'EXIT', _Exiting_Process_Id, _Reason} ->
                    stop()

            after
                5000 ->
                        %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (No se recibio respuesta del sequencer del mismo nodo)
                        stop()
                    

    
            end;



        {propuestaOrden, PropuestaOrden, IdentificadorMensaje, From} ->

            

            case dict:find(IdentificadorMensaje, DicMsgToSend) of 
                
                {ok, {CantPropuestasARecebir, ListaPropuestas, Nodos}} ->

                   

                    
                    %% Actualizar Datos
                    PropToReceive = CantPropuestasARecebir - 1,
                    ListaPropuestasPrima = ListaPropuestas ++ [PropuestaOrden],
                    ListaNuevaNodos = lists:delete(From, Nodos),
                    
                    if 
                        (PropToReceive == 0) ->
                            broadcast(ListaPropuestasPrima, IdentificadorMensaje),
                            NewDic = dict:erase(IdentificadorMensaje, DicMsgToSend),
                            aSender(CantMensajesEnviados,NewDic,CantNodos);   
            
                        true ->
                            
                            NewDic = dict:store(IdentificadorMensaje
                                    , {PropToReceive, ListaPropuestasPrima, ListaNuevaNodos}
                                    , DicMsgToSend),
                           
                            aSender(CantMensajesEnviados,NewDic,CantNodos)        
                    end;
                    
                error ->

                    %Estado inconsistente del sistema, MUERTE AL TRAIDOR. "CHE PARA YO NO TE ENVIE NADA" *Mata un Suizo
                    init:stop()
            end;
        
        {fin, From} -> 
            From ! senderFinOk,
            exit(normal);

        {nodedown, Node} ->

            NewCantNodos = contarElementos(nodes())+1,
            Porcentaje = NewCantNodos*100/CantNodos,
           
            
            if 
                Porcentaje >= ?PORCENTAJE ->

                   
                    KeyListMsgToSend = dict:fetch_keys(DicMsgToSend),
                    NewDicMsgToSend  = actualizarDiccionario(KeyListMsgToSend, DicMsgToSend, Node),
                   
                    aSender(CantMensajesEnviados,NewDicMsgToSend,CantNodos);

                true -> io:format("La cantidad de nodos es insuficiente para mantener el servicio ~n"),    
                        init:stop()
            end;
        {'EXIT', _Exiting_Process_Id, _Reason} ->
            %Murio un hermano, chau
            stop();
        _Error ->
            %Estado inconsistente del sistema, MUERTE AL TRAIDOR  (se recibio un mensaje re wadafak)
            init:stop()

    end.
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
%%broadcast: Selecciona la propuesta mas alta y notifica a todos los nodos
%
broadcast(ListaPropuestas, IdentificadorMensaje) ->

    PropuestaAcordada = lists:max(ListaPropuestas),
    lists:foreach(fun(X) -> {sequencer , X } ! {propuestaAcordada, IdentificadorMensaje, PropuestaAcordada} end, nodes()),
    sequencer ! {propuestaAcordada, IdentificadorMensaje, PropuestaAcordada}.
    

%
%% link_nodos: Conecta N nodos con el nombre "nodoN@nombreDeLaPc"
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

%
%%actualizarDiccionario : Actualiza el diccionaria de mensajes enviados cuando un nodo de la red
%                         se desconecta
%
actualizarDiccionario([],DicMsgToSend,_Node) ->
    DicMsgToSend;
actualizarDiccionario([Key|Keys], DicMsgToSend, Node) ->

    {PropToReceive, ListaPropuestas, ListaNodos} = dict:fetch(Key, DicMsgToSend),

    Prep = lists:member(Node, ListaNodos),
    
    if  
        Prep -> 
            NewListaNodos = lists:delete(Node,ListaNodos),
            Contador = PropToReceive - 1,
            
            if 
                Contador == 0 ->
                     
                    broadcast(ListaPropuestas, Key),
                    NewDic = dict:erase(Key,DicMsgToSend),
                    actualizarDiccionario(Keys,NewDic,Node);

               true ->  
                    NewDic = dict:store(Key,{Contador, ListaPropuestas, NewListaNodos},DicMsgToSend),
                    actualizarDiccionario(Keys,NewDic,Node)
       
            end;

        true -> actualizarDiccionario(Keys, DicMsgToSend, Node)
    end.

%
%%reacomodarDiccionario: Actualiza el orden de los mensajes cuando un nodo se cayo
%                        antes de enviar su mensaje acordado y ya se han almacenado
%                        numeros acordados mayores al mensaje perdido
reacomodarDiccionario(_NmsPerdidos, [], Diccionario, [{N,Msg}]) ->

    dict:store(N,Msg,Diccionario);

reacomodarDiccionario(NmsPerdidos, [], Diccionario, [{N,Msg} | Tail]) ->

    NewDiccionario = dict:store(N,Msg,Diccionario),
    reacomodarDiccionario(NmsPerdidos, [], NewDiccionario, Tail);

reacomodarDiccionario(NmsPerdidos, [Key|List], Diccionario, ToStore) ->
    Mensaje = dict:fetch(Key,Diccionario),
    Alpha = contarElementos(lists:filter(fun(X)-> X < Key end, NmsPerdidos)),
    NewToStore = ToStore ++ [{Key - Alpha, Mensaje}],
    reacomodarDiccionario(NmsPerdidos, List, Diccionario, NewToStore).

%
%%contarElementos: cuenta la cantidad de elementos de un lista
%
contarElementos([]) ->
    0;  
contarElementos([_Hd|Tl])->
    1 + contarElementos(Tl).

%
%% obtenerMensajesPerdidos: Apartir de los identificadores, obtenemos la lista de numeros
%                           perdidos
obtenerMensajesPerdidos([Key], DicMensajes) ->
    {_Msg, NumeroMensajePerdido} = dict:fetch(Key, DicMensajes),
    [NumeroMensajePerdido];
    
obtenerMensajesPerdidos([Key | Tail], DicMensajes) ->

    {_Msg, NumeroMensajePerdido} = dict:fetch(Key, DicMensajes),
    [NumeroMensajePerdido] ++ obtenerMensajesPerdidos(Tail, DicMensajes).

    


%
%%stop() se encarga de matar el nodo
%
stop() ->
    trySendFin(sequencer, sequencerFinOk),
    trySendFin(deliver, deliverFinOk),
    trySendFin(sender, senderFinOk),
    init:stop().




