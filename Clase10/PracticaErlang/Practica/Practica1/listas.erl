-module(listas).
-export([min/1,max/1,min_max/1]).


min([Hd]) -> Hd;
min([Hd|Tl]) ->
 Rest = min(Tl),
 if 
    Hd < Rest -> Hd;
    true -> Rest
 end.

max([Hd]) -> Hd;
max([Hd|Tl]) -> 

    Rest = max(Tl),

    if Hd > Rest ->
       Hd;

    true -> 
       Rest

    end.

min_max([Hd])    -> {Hd,Hd};
min_max(Xs) -> {min(Xs),max(Xs)}.

