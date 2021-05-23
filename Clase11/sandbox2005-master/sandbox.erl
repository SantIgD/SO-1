-module(sandbox).
-export([play/0, player/0]).

show_mailbox() ->
    receive
        Msg ->
            io:format("Player[~p]: ~p ~n", [self(), Msg] ),
            show_mailbox()
    after 0 -> io:format("[~p] Buzon vacio ~n", [self()])
    end.


player() ->
    broadcast:registrar(),
    broadcast:broadcast("Holis!!"),
    mailbox:sleep(1000),
    show_mailbox(),
    ok.

play() ->
    broadcast:start(),
    Pid1 = spawn(?MODULE, player,[]),
    Pid2 = spawn(?MODULE, player,[]),
    Pid3 = spawn(?MODULE, player,[]),
    ok.
