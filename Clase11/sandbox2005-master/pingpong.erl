-module(pingpong).
-export([pong_init/0,ping/2, play/0]).

play() ->
    Pongid = spawn(?MODULE, pong_init, []),
    spawn(?MODULE, ping, [10, Pongid]),
    Pongid.

pong_init() ->
    process_flag(trap_exit, true),
    pong().

pong() ->
    receive
        fin -> ok;
        {ping, Pid} ->
            io:format("Pong!~n"),
            Pid ! {pong, self()},
            pong();
        {'EXIT', Ra, Pid } ->
            io:format(">Murio ~p por ~p ~n",[Ra,Pid]),
            exit(normal)
    end.

%% ping/1
ping(0) ->
    receive
        {pong, Pid} ->
            Pid ! fin,
            ok
    end;

ping(5) ->
    exit(covid);
ping(N) ->
    receive
        {pong, Pid} ->
            io:format("Ping: ~p ~n",[N]),
            Pid ! {ping, self()},
            ping(N-1)
    end.

%% ping/2
ping(N, Pongid) ->
    link(Pongid),
    Pongid ! {ping, self()},
    ping(N).
