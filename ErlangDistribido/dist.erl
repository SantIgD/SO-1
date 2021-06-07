-module(dist).
-export([echoNode/1]).

echoNode(Pid) ->
    io:format("Holis soy ~p ~n", [self()]),
    io:format(lists:list_to_pid("<0.86.0>"), "test ~p ~n",[self()]),
    Pid ! {node(), self()}.
