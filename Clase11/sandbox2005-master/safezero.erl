-module(safezero).
-export([safezerodiv/2, covid/1]).

covid(N) ->
    if
        N == 0 ->
            throw(die);
        true ->
            N
    end,
    9 .

safezerodiv(M,N) ->
    case catch (M / covid(N)) of
        {'EXIT', {badarith, _Args}} ->
           io:format("Div Zero Capturada~n"),
           err;
        die ->
            io:format("Morto ~n");
        Res -> Res
    end.
