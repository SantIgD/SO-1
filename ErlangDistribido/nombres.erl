-module(nombres).
-export([start/0]).
-export([nombres/1]).

start() ->
    register(serv,spawn(?MODULE,nombres,[[]])).

nombres(L) ->
    receive
        {add, S} ->
            nombres([S | L]);
        {get, Pid} ->
            Pid ! {ok, L},
            nombres(L);
        {show, Pid} ->
            io:format(standard_io,"Show: ~p ~n",[L]),
            nombres(L);
        fin -> ok
    end.
