-module(conjunto).
%% Funciones de control
-export([start/0, stop/0]).
%% Librería de acceso.
-export([addE/1, getS/0]).
%% Proceso loop.
-export([initSet/1]).

-define(Dbg(Str),io:format("[DBG]~p:" ++ Str,[?FUNCTION_NAME])).
-define(Dbg(Str,Args),io:format("[DBG]~p:" ++ Str,[?FUNCTION_NAME|Args])).

start() ->
    N = nodes(),
    if N == [] ->
            register(servset, spawn(?MODULE, initSet,[primero]));
       true ->
            register(servset, spawn(?MODULE, initSet,[N]))
    end,
    ok.

stop()->
    servset ! fin,
    unregister(servset),
    ok.

addE(E) ->
    servset ! {addE, E, self()},
    receive
        ok ->
            ok;
        {error, Reason} ->
            ?Dbg("~p~n",[Reason]),
            error
    end.

getS() ->
    servset ! {reqSet, self()},
    receive
        {ok, Set} ->
            Set;
        {error, Reason} ->
            ?Dbg("~p~n",[Reason]),
            error
    end.

%% spawn(initSet, primero|Nds)
%% Cree un nuevo Actor, Actor1
%% Actor1[initSet(primero|Nds)]

%% Asumimos que los nodos se van agregando secuencialmente.
%% Pensar un mecanismo para cuando no sea así.
%%
initSet(primero) ->
    ?Dbg("Se inicia el servidor como primer nodo.~n"),
    loopC(sets:new());

initSet(Nodes) ->
    ?Dbg("Comenzamos el proceso de pedido del conjunto distribuido.~n"),
    %% Pedimos a todos los conjuntos ->
    %% \forall n \in Nodes, n ! {reqSet, self()}.
    lists:foreach(fun (X) ->
                          {servset, X} ! {reqSetNode, self()},
                          monitor_node(X, true)
                  end, Nodes),
    %% Iniciamos en la unión.
    ?Dbg("Pasamos a esperar las respuestas~n"),
    preLoop(sets:from_list(Nodes),sets:new()).

preLoop(Nodes,Set) ->
    Test = sets:is_empty(Nodes),
    if Test -> loopC(Set);
       true ->
        receive
            {envSetNode, SetE, Who} ->
                ?Dbg("recibimos de ~p ~n",[Who]),
                monitor_node(Who, false),
                preLoop( sets:del_element(Who,Nodes)
                       , sets:union(SetE, Set));
            {addP, E} ->
                ?Dbg("Llega por la red ~p",[E]),
                preLoop(Nodes, sets:add_element(E, Set));
            {addE, E, PId} ->
                ?Dbg("agregado ~p ~n",[E]),
                propagar({addP,E}),
                PId ! ok,
                preLoop(Nodes, sets:add_element(E, Set));
            {nodedown, Who} ->
                ?Dbg("Se cayó ~p ~n",[Who]),
                monitor_node(Who, false),
                preLoop(sets:del_element(Who, Nodes), Set)
        end
    end.

propagar(Msg) ->
    ?Dbg("Propagando ~p ~n",[Msg]),
    lists:foreach(fun (X) ->
                          {servset , X} ! Msg
                  end, nodes()).

%%
loopC(Set) ->
    ?Dbg("Esperando Petición~n"),
    receive
        {addP, E} ->
            ?Dbg("Llega por la red ~p.~n",[E]),
            loopC(sets:add_element(E, Set));
        {addE, E, PId} ->
            ?Dbg("agregado ~p ~n",[E]),
            propagar({addP,E}),
            PId ! ok,
            loopC(sets:add_element(E, Set));
        {reqSetNode, PId} ->
            ?Dbg("Compartiendo el estado con nodo ~p ~n",[PId]),
            PId ! {envSetNode, Set, node()},
            loopC(Set);
        {reqSet, PId} ->
            ?Dbg("mostrando set a ~p ~n",[PId]),
            PId ! {ok, Set},
            loopC(Set);
        fin ->
            ?Dbg("Saliendo ~n"),
            ok;
        _Err ->
            ?Dbg("algo extraño está ocurriendo~n"),
            loopC(Set)
    end.
