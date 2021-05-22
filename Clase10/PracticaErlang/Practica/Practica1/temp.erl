-module(temp).
-export([f2c/1,c2f/1,convertir/1]).



f2c(F) -> 
    io:format("~p[F] <-> ~p[C] ~n",[F,(F-32)*5/9]).

c2f(C) ->
    io:format("C:~p <-> F:~p ~n",[C,(C*9/5)+32]).


convertir({f,Valor}) -> f2c(Valor);
convertir({c,Valor}) -> c2f(Valor).
