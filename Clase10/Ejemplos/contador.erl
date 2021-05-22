-module(contador).
-export([iniciar/0, terminar/1]).
-export([incrementar/1, valor/1, bucle/1]).

%% Modulo Contador


iniciar() -> spawn(contador, bucle, [0]).

terminar(Contador) -> Contador ! fin.

%% incr
incrementar(Contador) -> Contador ! inc.

%% valor
valor(Contador) ->

    Contador ! {val, self()},
    
    receive

        N -> N

    end.

%% dec
decrementar(Contador) -> Contador ! dec.


%-----------------------------------------
%-----------------------------------------

%% Estado interno del contador es un valor
bucle(N) ->

    receive

        fin -> ok ;
        
        inc -> bucle(N+1);
        
        {val, Pid} -> Pid ! N,
                      bucle(N);

        dec -> bucle(N-1)
    end.
    
