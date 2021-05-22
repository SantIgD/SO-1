-module(pingpong).
-export([play/0,pong/0,ping/1]).

% Pong
pong() ->

    receive
        
        {0 , Pid} ->
            io:format("Final Pong!~n"),
            Pid ! {fin, self()},
            pongok;

        {N , Pid} ->
            io:format("Pong! Recv: ~p ~n",[N]),
            Pid ! {(N-1) , self()},
            pong()
    end.


ping(PidPong) ->

    receive

        {fin , PidPong } -> pingok;
        
        {N , PidPong} ->
            io:format("Ping! ~n"),
            PidPong ! {N , self()},
            ping(PidPong)

        %% _ -> ping(PidPong)
    end.

play() ->

    PIdPong = spawn(pingpong, pong, []),
    PIdPing = spawn(pingpong, ping, [PIdPong]),

    PIdPong ! { 10 , PIdPing },
    PIdPing ! { fin, self()  },

    playok.
