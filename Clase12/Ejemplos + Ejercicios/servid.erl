-module(servid).
-include("servid.hrl").

%%%%%%
%% Pequeño ejercicio de clase
%% el servidor aceptara 4 tipos de pedidos:
%% + nuevoId(Nombre, PidResp) -> Se generará un nuevo identificar para `Nombre` y se responderá al cliente.
%% + buscarId(Id, PidResp) -> Se responde a `PidResp` el nombre asociado a `Id`.
%% + verLista(PidResp) -> Se envía a `Pidresp` la lista de pares (Id,Nombre).
%% + finalizar(PidResp) -> Se finaliza el servicio y se responde con un `ok`.
%%

%% Creación y eliminación del servicio
-export([iniciar/0, finalizar/0]).

%% Servidor
-export([serverinit/1]).

%% Librería de Acceso
-export([nuevoNombre/1, quienEs/1, listaDeIds/0]).

% Iniciar crea el proceso servidor, y devuelve el PId.
iniciar() ->

    register(servidorIds,
             spawn( ?MODULE
                  , serverinit
                  , [self()])),

    receive
        ok -> io:format("Se inicio el servidor correctamente \n")
    end,
    ok.

%%%%%%%%%%%%%% Librería de Acceso
%%
%% Dado un nombre y un servidor le pide que cree un identificador
%% único.
nuevoNombre(Nombre) ->
    servidorIds ! {nuevoId, Nombre, self()},
    receive
        {nombreRegistrado, N} -> io:format("El nombre ~p se registro correctamente\n",[N]);
        _MENSAJE  -> io:format("El mensaje recibido fue >~p<\n",[_MENSAJE])
    end.

%% Función que recupera el nombre desde un Id
quienEs(Id) ->
    servidorIds ! {buscarId, Id, self()},
    receive
        {idHallado, Nm} -> io:format("El nombre ~p es la clave del id ~p\n",[Nm,Id]);
        _  -> error
    end.

%% Pedimos la lista completa de nombres e identificadores.
listaDeIds() ->
    servidorIds ! {verLista, self()},
    receive
        {listaRecuperada, List} -> io:format("La lista que corresponde al mapa es ~p\n",[List]);
        _ -> error
    end.

% Ya implementada :D!
finalizar() ->
    servidorIds ! {finalizar, self()},
    unregister(servidorIds),
    receive
        finalizado -> io:format("El servidor se cerro correctamente\n"); 
        _ -> error
    end.

%%%%%%%%%%% Servidor
%%%
%% Función de servidor de nombres.

serverinit(PInit) ->
    PInit ! ok,
    %% Comenzamos con un mapa nuevo, y un contador en 0.
    servnombres(maps:new(), 0).

servnombres(Map, N) ->

    receive

        %% Llega una petición para crear un Id para nombre
        #nuevoId{nombre = Nombre, clientid = ClientID} ->
            io:format("Entro al nuevo nickname\n"),
            ClientID ! {nombreRegistrado, Nombre},
            servnombres(maps:put(Nombre,ClientID,Map), N + 1);

        %% Llega una petición para saber el nombre de tal Id
        {buscarId, NId, ClientID} ->
            case maps:find(NId, Map) of
                {idHallado, Nombre} -> ClientID ! {ok, Nombre};
                error -> ClientID ! {err, notfound}
            end,
            servnombres(Map,N);

        %% Entrega la lista completa de Ids con Nombres.
        {verLista, ClientID} ->
            ClientID ! {listaRecuperada, maps:to_list(Map)},
            servnombres(Map,N);
        
        %% Cerramos el servidor. Va gratis
        
        {finalizar, ClientID } ->
            ClientID ! finalizado;
        
        _ -> io:format("DBG: otras cosas~n"),
             servnombres(Map,N)
    end.
