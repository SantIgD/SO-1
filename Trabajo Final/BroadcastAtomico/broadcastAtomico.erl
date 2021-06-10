-module(broadcastAtomico).

%%-include("broadcastAtomico.hrl").

%%Libreria de control
-export([start/0,stop/0]).

%%Libreria de acceso
-export([aBroadcast/1]).

-export([aDeliver/0,aSequencer/5,aSender/2]).

-define(Nodos,4).

-record(paqueteSinOrden, {msg,identificador,ordenPropuesto}).

start() ->

    register(sequencer, spawn(?MODULE, aSequencer,[dict:new(),0,0,0,0])),
    register(deliver,   spawn(?MODULE, aDeliver  ,[])),
    register(sender,    spawn(?MODULE, aSender   ,[0,dict:new()])),
    
    ok.

stop() ->

    sequencer ! fin,
    deliver   ! fin,
    sender    ! fin,

    unregister(sequencer),
    unregister(deliver),
    unregister(sender),

    ok.

aBroadcast(Mensaje) ->
   
    sender ! {msg, Mensaje},
    ok.


contarElementos([_Hd])->
    1;
contarElementos([_Hd|Tl])->
    1 + contarElementos([Tl]).


aSender(CantMensajesEnviados,DicMsgToSend) ->

    receive 
        
        fin -> exit(normal);

        {msg, Mensaje} -> 

        sequencer ! {crearPaqueteSO, Mensaje, CantMensajesEnviados + 1},

            receive

                {paqueteSO,PaqueteSO} ->
                        Nodes = nodes(),
                        CantPropuestasARecebir = contarElementos(Nodes),

                        NewDic = dict:store(PaqueteSO#paqueteSinOrden.identificador
                                , {CantPropuestasARecebir,[]}
                                , DicMsgToSend),

                    lists:foreach(fun(X) -> {sequencer , X } ! {askingForOrder,PaqueteSO} end,Nodes),
                    
                    aSender(CantMensajesEnviados+1,NewDic)
            end;

        {propuestaOrden, PropuestaOrden, IdentificadorMensaje} ->

            case dict:find(IdentificadorMensaje, DicMsgToSend) of 

                {ok, Value} ->

                    {CantPropuestasARecebir,ListaPropuestas} = Value,
                    PropToReceive = CantPropuestasARecebir - 1,
                    ListaPrima = ListaPropuestas ++ [PropuestaOrden],

                    if 
                        (PropToReceive == 0) ->

                                PropuestaAcordada = lists:max(ListaPrima),
                                NewDic = dict:erase(IdentificadorMensaje, DicMsgToSend),
                                lists:foreach(fun(X) -> {sequencer , X } ! {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada} end,nodes()),
                                sequencer ! {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada},
                                aSender(CantMensajesEnviados,NewDic);   
                
                        true->
                            NewDic = dict:store(IdentificadorMensaje
                                    , {PropToReceive,ListaPrima}
                                    , DicMsgToSend),
                            aSender(CantMensajesEnviados,NewDic)        
                    end;
                    
                error ->
                    error %% se muere, como debe ser... por inepto 
            end


    end.


aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto, OrdenActual, TO)-> 

    receive

        {crearPaqueteSO, Mensaje, CantMensajesEnviados} -> 

            PaqueteSO = #paqueteSinOrden{msg = Mensaje
                        , identificador = {node(),CantMensajesEnviados}
                        , ordenPropuesto = OrdenMaximoPropuesto},


            sender ! {paqueteSO,PaqueteSO},
                        

            NewDic    = dict:store(PaqueteSO#paqueteSinOrden.identificador
                        , {PaqueteSO#paqueteSinOrden.msg,PaqueteSO#paqueteSinOrden.ordenPropuesto}
                        , DicMensajes),

            aSequencer(NewDic,OrdenMaximoAcordado,OrdenMaximoPropuesto + 1, OrdenActual, TO);
    

        {askingForOrder, Paquete} when is_record(Paquete,paqueteSinOrden) -> 
            
            PropuestaOrden = lists:max([OrdenMaximoPropuesto,Paquete#paqueteSinOrden.ordenPropuesto]) + 1,

            {Nodo,_} = Paquete#paqueteSinOrden.identificador,            
            {sender, Nodo} ! {propuestaOrden, PropuestaOrden,Paquete#paqueteSinOrden.identificador},

            NewDic = dict:store(Paquete#paqueteSinOrden.identificador
                        ,{Paquete#paqueteSinOrden.msg, PropuestaOrden}
                        , DicMensajes),

            %%ojo aca
            aSequencer(NewDic, OrdenMaximoAcordado, OrdenMaximoPropuesto, OrdenActual, TO);


        {propuestaAcordada,IdentificadorMensaje,PropuestaAcordada} ->

                NewOrdenMaxAcor = lists:max([PropuestaAcordada,OrdenMaximoAcordado]),
                case dict:find(IdentificadorMensaje, DicMensajes) of

                {ok, {Mensaje,_OrdenPropuesto}} ->

                    AuxDic = dict:erase(IdentificadorMensaje, DicMensajes),
                    NewDic = dict:store(PropuestaAcordada
                                    , Mensaje
                                    , AuxDic),
                    
                    aSequencer(NewDic,NewOrdenMaxAcor,OrdenMaximoPropuesto, OrdenActual, 0);
                   
                error ->
                    error;

                Other -> io:format("ME LLEGO ~p~n",[Other])
                end
        after 
            
            %% Tratamos de mandar a Deliver
            TO -> 
                case dict:find(OrdenActual+1, DicMensajes) of

                    {ok, Msg} ->
                                deliver ! Msg,
                                NewDic = dict:erase(OrdenActual+1, DicMensajes),
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