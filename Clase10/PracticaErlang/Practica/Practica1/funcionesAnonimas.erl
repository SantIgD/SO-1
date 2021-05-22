-module(funcionesAnonimas).
-export([fecha_actual/0]).


fecha_actual() -> fecha_actual(date()).

fecha_actual({Year,Month,Day}) -> fecha_actual([Day,Month,Year]);

fecha_actual([Hd]) -> put_sep(Hd);
fecha_actual([Hd|Tail]) -> put_sep(Hd) ++ fecha_actual(Tail).


put_sep(X) ->

    if
        X < 10 ->
            "0" ++ integer_to_list(X) ++ "/";
             
        X > 99 ->
            last2(integer_to_list(X));
            
        true -> integer_to_list(X) ++ "/"
                
    end.

last2([Hd1,Hd2]) -> [Hd1,Hd2];
last2([_|Tail]) -> last2(Tail).


