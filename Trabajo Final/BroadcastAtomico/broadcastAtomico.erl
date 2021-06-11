-module(broadcastAtomico).

%%-include("broadcastAtomico.hrl").

%%Libreria de control
-export([start/0,stop/0]).

%%Libreria de acceso
-export([aBroadcast/1]).

-export([aDeliver/0,aSequencer/5,aSender/2]).
-export([contarElementos/1]).
-define(Nodos,4).

-record(paqueteSinOrden, {msg,identificador,ordenPropuesto}).

%%start() se encarga de inicializar el nodo
start() ->

    io:format("Se inicia el nodo ~p~n",[node()]),
    register(sequencer, spawn(?MODULE, aSequencer,[dict:new(),0,0,0,0])),
    register(deliver,   spawn(?MODULE, aDeliver  ,[])),
    register(sender,    spawn(?MODULE, aSender   ,[0,dict:new()])),
    
    ok.

%%stop() se encarga de parar el nodo
stop() ->

    sequencer ! fin,
    deliver   ! fin,
    sender    ! fin,

    unregister(sequencer),
    unregister(deliver),
    unregister(sender),

    ok.

aBroadcast(Mensaje) ->
    io:format("Se quiere mandar el mensaje >~p<~n",[Mensaje]),
    sender ! {msg, Mensaje},
    ok.

%%contarElementos cuenta la cantidad de elementos de un lista
contarElementos([]) ->
    0;  
contarElementos([_Hd|Tl])->
    1 + contarElementos(Tl).


aSender(CantMensajesEnviados,DicMsgToSend) ->
    
    receive 
        fin -> exit(normal);
        {msg, Mensaje} -> 

            io:format("[sender] Se inicia el protocolo de envio de >~p<~n",[Mensaje]),

            sequencer ! {crearPaqueteSO, Mensaje, CantMensajesEnviados + 1},

            receive
                {paqueteSO,PaqueteSO} ->
                        io:format("[sender] Recibimos el paqueteSinOrden ~p~n",[PaqueteSO]),
                        Nodes = nodes(),
                        CantPropuestasARecebir = contarElementos(Nodes),

                        io:format("[sender] Diccionario sin modificar: ~p~n",[dict:to_list(DicMsgToSend)]),

                        NewDic = dict:store(PaqueteSO#paqueteSinOrden.identificador
                                , {CantPropuestasARecebir,[]}
                                , DicMsgToSend),

                        io:format("[sender] Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),

                    lists:foreach(fun(X) -> {sequencer , X } ! {askingForOrder,PaqueteSO} end,Nodes),
                    
                    aSender(CantMensajesEnviados+1,NewDic)
            end;

        {propuestaOrden, PropuestaOrden, IdentificadorMensaje} ->

            io:format("[sender] Se recibio la propuesta de orden >~p< para el identificador >~p<~n",[PropuestaOrden,IdentificadorMensaje]),

            case dict:find(IdentificadorMensaje, DicMsgToSend) of 
                
                {ok, Value} ->

                    io:format("[sender]  El identificador >~p< esta asociado al mensaje ~p con la propuesta ~p ~n",[IdentificadorMensaje,Value,PropuestaOrden]),


                    {CantPropuestasARecebir,ListaPropuestas} = Value,
                    PropToReceive = CantPropuestasARecebir - 1,
                    ListaPrima = ListaPropuestas ++ [PropuestaOrden],

                    if 
                        (PropToReceive == 0) ->

                                PropuestaAcordada = lists:max(ListaPrima),
                                io:format("[sender][listaPropuesta] Diccionario sin modificado: ~p~n",[dict:to_list(DicMsgToSend)]),
                                NewDic = dict:erase(IdentificadorMensaje, DicMsgToSend),
                                io:format("[sender][listaPropuesta] Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                                lists:foreach(fun(X) -> {sequencer , X } ! {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada} end,nodes()),
                                sequencer ! {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada},
                                aSender(CantMensajesEnviados,NewDic);   
                
                        true->
                            io:format("[sender][sumandoPropuestas] Diccionario sin modificado: ~p~n",[dict:to_list(DicMsgToSend)]),
                            NewDic = dict:store(IdentificadorMensaje
                                    , {PropToReceive,ListaPrima}
                                    , DicMsgToSend),
                            io:format("[sender][sumandoPropuestas] Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                            aSender(CantMensajesEnviados,NewDic)        
                    end;
                    
                error ->
                   io:format("[Sender] El identificador ~p no corresponde a ningun mensaje~n",[IdentificadorMensaje])  %% se muere, como debe ser... por inepto 
            end


    end.


aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual, TO)-> 

    receive

        {crearPaqueteSO, Mensaje, CantMensajesEnviados} -> 

            PaqueteSO = #paqueteSinOrden{msg = Mensaje
                        , identificador = {node(),CantMensajesEnviados}
                        , ordenPropuesto = OrdenMaximoPropuesto},


            sender ! {paqueteSO,PaqueteSO},
                        
            io:format("[sequencer][Local]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),

            NewDic    = dict:store(PaqueteSO#paqueteSinOrden.identificador
                        , {PaqueteSO#paqueteSinOrden.msg,PaqueteSO#paqueteSinOrden.ordenPropuesto}
                        , DicMensajes),
            io:format("[sequencer][Local] Diccionario  modificado: ~p~n",[dict:to_list(NewDic)]),
            aSequencer(NewDic,OrdenMaximoAcordado,OrdenMaximoPropuesto + 1, OrdenActual, TO);
    

        {askingForOrder, Paquete} when is_record(Paquete,paqueteSinOrden) -> 
            
            PropuestaOrden = lists:max([OrdenMaximoPropuesto,Paquete#paqueteSinOrden.ordenPropuesto]) + 1,

            {Nodo,_} = Paquete#paqueteSinOrden.identificador,            
            {sender, Nodo} ! {propuestaOrden, PropuestaOrden,Paquete#paqueteSinOrden.identificador},
            
            io:format("[sequencer][askingForOrder]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),
            NewDic = dict:store(Paquete#paqueteSinOrden.identificador
                        ,{Paquete#paqueteSinOrden.msg, PropuestaOrden}
                        , DicMensajes),
            io:format("[sequencer][askingForOrder]Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
            %%ojo aca
            aSequencer(NewDic, OrdenMaximoAcordado, PropuestaOrden, OrdenActual, TO);


        {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada} ->

                NewOrdenMaxAcor = lists:max([PropuestaAcordada,OrdenMaximoAcordado]),
                case dict:find(IdentificadorMensaje, DicMensajes) of
                
                {ok, {Mensaje,_OrdenPropuesto}} ->
                    io:format("[sequencer][propuestaAcordada]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),
                    AuxDic = dict:erase(IdentificadorMensaje, DicMensajes),
                    NewDic = dict:store(PropuestaAcordada
                                    , Mensaje
                                    , AuxDic),
                    io:format("[sequencer][propuestaAcordada]Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                    aSequencer(NewDic,NewOrdenMaxAcor,OrdenMaximoPropuesto, OrdenActual, 0);
                   
                error ->
                   io:format("[sequencer] El identificador ~p no esta asociado a ningun mensaje ~n",[IdentificadorMensaje]);

                Other -> io:format("ME LLEGO ~p~n",[Other])
                end
        
    after 
        
        %% Tratamos de mandar a Deliver
        TO -> 
            case dict:find(OrdenActual+1, DicMensajes) of
                {ok, Msg} ->
                            deliver ! Msg,
                            io:format("[sequencer][TO]Diccionario sin modificar: ~p~n",[dict:to_list(DicMensajes)]),
                            NewDic = dict:erase(OrdenActual+1, DicMensajes),
                            io:format("[sequencer][TO]Diccionario modificado: ~p~n",[dict:to_list(NewDic)]),
                            aSequencer(NewDic,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual + 1, 0);
        
                error -> aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual,infinity)
            end     

    end.

aDeliver() ->
    receive
        fin -> exit(normal);
        M ->
            io:format("Deliver : ~p ~n",[M]),
            aDeliver()
    end.


%%Sharing Terminal: Could not start terminal process C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe: The directory name is invalid.