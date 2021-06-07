-module(llamada_remota).
-export([llamada/2]).

-define(TO, 100).

llamada(Node, Message) ->
    {facserv, Node} ! {self(), Message},
    monitor_node(Node, true),
    receive
        {ok, Res} ->
            Res;
        {errordown, Node} -> {error, nodomurio}
    end.
    %%monitor_node(Node, false).
