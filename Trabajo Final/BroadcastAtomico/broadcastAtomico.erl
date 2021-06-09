-module(broadcastAtomico).

-include("broadcastAtomico.hrl").

%%Libreria de control
-export([start/0,stop/0]).

%%Libreria de acceso
-export([aBroadcast/1]).

-export([aDeliver/0,aSequencer/3,aSender/0]).

-define(Nodos,4).

start() ->

    register(sequencer, spawn(?MODULE, aSecuencer,[noMensaje,0,0,dict:new()])),
    register(deliver,   spawn(?MODULE, aDeliver  ,[])),
    register(sender,    spawn(?MODULE, aSender   ,[])),
    ok.

stop() ->
    ok.


proposalColector(Lista)
receive
->ok.

aSender() ->

    receive 
        
        fin -> exit(normal);

        Mensaje -> 

        sequencer ! getPaquetInfo,

        receive

            {paquetInfo,OrdenMaximo,CantMensajesEnviados} ->
                    PaqueteSO = #paqueteSinOrden{msg = Mensaje
                                               ,claveMensaje = {node(),CantMensajesEnviados}
                                               ,ultimoOrdenAcordado = OrdenMaximo},

                    lists:foreach(fun(X) -> {sequencer , X } ! {askingForOrder,PaqueteSO} end,nodes()),
                    FinalPropuesta=lists:max(proposalColector([]))
        end





    end.

aBroadcast(Mensaje) ->
   
    sender ! Mensaje,
    ok.


aDeliver() ->
    receive
        fin -> exit(normal);
        M ->
            io:format("Deliver : ~p ~n",[M]),
            aDeliver()
    end.

aSequencer(CantMensajesEnviados,OrdenMaximo,DicMensajesSeq)-> 

    receive

        {askingForOrder,Paquete} when is_record(Paquete,paqueteSinOrden) -> 

            PropuestaOrden = lists:max([OrdenMaximo,Paquete#paqueteSinOrden.ultimoOrdenAcordado]) + 1,
            sender ! {propuestaOrden, PropuestaOrden};

        getPaquetInfo -> sender ! {paquetInfo,OrdenMaximo,CantMensajesEnviados}
    end.
