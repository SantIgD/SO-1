-module(listas).
-export([play/0]).

primerosDos(Xs) ->
    [A, B | Resto ] = Xs,
    io:format("Me llego una lista cuyos primeros dos elementos son~n"
             "\t + ~p ~n \t + ~p~n",[A, B]).


ultimo([Head]) -> io:format("El ultimo elemento de la lista es: ~p ~n",[Head]);
ultimo([Head | Tail] ) -> ultimo(Tail).


play() ->
    Xs = [1 , 2 ,3 , banana, manzana, pera],
    primerosDos(Xs),
    ultimo(Xs). % deberia ser `pera`
