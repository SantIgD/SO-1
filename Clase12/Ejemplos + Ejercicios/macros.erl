-module(macros).

-export([play/0]).

%%c(macros,[{d,bandera}]). para cargar la bandera

-ifdef(bandera).
    -define(PUNTO,1).
    -define(fun1(Arg1),Arg1*Arg1).
-else.
    -define(PUNTO,2).
    -define(fun1(Arg1),Arg1*4).
-endif.

play()->

    io:format("Esta es la macro ~p y la funcion dio como resultado ~p\n",[?PUNTO,?fun1(?PUNTO)]),
    ok.
