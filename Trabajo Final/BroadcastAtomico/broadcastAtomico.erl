-module(broadcastAtomico).

-include("broadcastAtomico.hrl").

%%Libreria de control
-export([start/0,stop/0]).

%%Libreria de acceso
-export([aBroadcast/1]).

-export([aDeliver/0,aSequencer/3,aSender/2]).

-define(Nodos,4).

start() ->

    register(sequencer, spawn(?MODULE, aSecuencer,[dict:new(),0,0])),
    register(deliver,   spawn(?MODULE, aDeliver  ,[])),
    register(sender,    spawn(?MODULE, aSender   ,[0,dict:new()])),
    ok.

stop() ->
    ok.


proposalColector(Lista)
    receive

    ->ok
    end.

aBroadcast(Mensaje) ->
   
    sender ! Mensaje,
    ok.


contarElementos([Hd])->
    1.
contarElementos([Hd|Tl])->
    1 + contarElementos([Tl]).


aSender(CantMensajesEnviados,DicMsgToSend) ->

    receive 
        
        fin -> exit(normal);

        Mensaje -> 

        sequencer ! {getPaquetInfo, Mensaje, CantMensajesEnviados + 1},

        receive

            {paquetInfo,OrdenMaximoAcordado} ->
                    PaqueteSO = #paqueteSinOrden{msg = Mensaje
                                               ,claveMensaje = {node(),CantMensajesEnviados}
                                               ,ultimoOrdenAcordado = OrdenMaximoAcordado},

                    CantPropuestasARecebir = contarElementos(nodes()),

                    NewDic = dict:append(PaqueteSO#paqueteSinOrden.claveMensaje
                            , {CantPropuestasARecebir,[]}
                            , DicMsgToSend),

                lists:foreach(fun(X) -> {sequencer , X } ! {askingForOrder,PaqueteSO} end,nodes())
        end,

        aSender(CantMensajesEnviados,NewDic);

        
        {propuestaOrden, PropuestaOrden, IdentificadorMensaje} ->

            case dict:find(IdentificadorMensaje, DicMsgToSend)
                {ok, Value} ->
                    {CantPropuestasARecebir,ListaPropuestas} = Value,
                    PropToReceive = CantPropuestasARecebir - 1,
                    ListaPrima = ListaPropuestas ++ [PropuestaOrden],

                    if 
                        (PropToReceive == 0) ->
                                            PropuestaFinal = lists:max(ListaPrima),
                                            NewDic = dict:erase(IdentificadorMensaje, DicMsgToSend)
                                            lists:foreach(fun(X) -> {sequencer , X } ! {propuestaFinal,PropuestaFinal} end,nodes()),
                                            sequencer ! {propuestaFinal,PropuestaFinal},
                                            aSender(CantMensajesEnviados,NewDic);   

                                            
                        true->
                            NewDic = dict:store(IdentificadorMensaje
                                    , {PropToReceive,ListaPrima}
                                    , DicMsgToSend),
                            aSender(CantMensajesEnviados,NewDic)        
                    end;
                    
                error ->
                    error. %% se muere, como debe ser... por inepto 
            end


    end.


aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto)-> 

    receive
        {askingForOrder,Paquete} when is_record(Paquete,paqueteSinOrden) -> 
            %% [clave = {Nodo,CantMensajesEnviados}] => {Mensaje , propuestaValor}   , valorDefinitivo => Mensaje
            PropuestaOrden = lists:max([OrdenMaximoPropuesto,Paquete#paqueteSinOrden.ultimoOrdenAcordado]) + 1,
            {Nodo,_} = Paquete#paqueteSinOrden.claveMensaje,            
            {sender, Nodo} ! {propuestaOrden, PropuestaOrden,Paquete#paqueteSinOrden.claveMensaje},

            dict:append(PaqueteSO#paqueteSinOrden.claveMensaje
                            , {PaqueteSO#paqueteSinOrden.msg, PropuestaOrden}
                            , DicMensajes),

            aSequencer(DicMensajes, OrdenMaximoAcordado, PropuestaOrden);

        {getPaquetInfo, Mensaje, CantMensajesEnviados} -> sender ! {paquetInfo,OrdenMaximoAcordado},
                        PaqueteSO = #paqueteSinOrden{msg = Mensaje
                                               ,claveMensaje = {node(),CantMensajesEnviados}
                                               ,ultimoOrdenAcordado = OrdenMaximoPropuesto},

                        dict:append(PaqueteSO#paqueteSinOrden.claveMensaje
                            , {PaqueteSO#paqueteSinOrden.msg,PaqueteSO#paqueteSinOrden.ultimoOrdenAcordado}
                            , DicMensajes),

                        aSequencer(DicMensajes,OrdenMaximoAcordado,OrdenMaximoPropuesto + 1)
    end.

aDeliver() ->

    receive
        fin -> exit(normal);
        M ->
            io:format("Deliver : ~p ~n",[M]),
            aDeliver()
    end.


%%Sharing Terminal: Could not start terminal process C:\Windows\System32\WindowsPowerShell\v1.0\powershell.exe: The directory name is invalid.