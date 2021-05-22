-module(min).
-export([min/1]).

min([Hd]) -> Hd;
min([Hd|Tl]) ->
 Rest = min(Tl),
 if 
    Hd < Rest -> Hd;
    true -> Rest
 end.