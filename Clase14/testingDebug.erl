-module(testingDebug).


-export([play/0]).

-define(Dbg(Str),io:format("[DBG]~p:" ++ Str ++ "~n",[?FUNCTION_NAME])).
-define(Dbg(Str,Args),io:format("[DBG]~p:" ++ Str ++ "~n",[?FUNCTION_NAME|Args])).



play() -> 
    ?Dbg("Hola soy el primer print"),
    ?Dbg("Hola soy el segundo print ~p / ~p / ~p / ~p ",[pepeElBug,2,3,4]).