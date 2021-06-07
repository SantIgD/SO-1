-module(servfac).
-export([init/0,stop/0,imprime/1]).
-export([loopFac/0]).

init() ->
    register(facserv, spawn(?MODULE, loopFac,[])).

stop() ->
    facserv ! fin,
    unregister(facserv).

%% Loop Servicio
loopFac() ->
    receive
        {Pid, N} ->
            Pid ! {ok, fac(N)},
            loopFac();
        fin -> ok;
        _ -> loopFac()
    end.

imprime(Valor)->
    io:format("valor del factorial ~p",[fac(Valor)]).

fac(0) ->
    1;
fac(N) when N > 0 ->
    N * fac(N-1);
fac(_) ->
    err.

%rpc:call('carlos@DESKTOP-SA7FBFS',servfac,fac,[5]).