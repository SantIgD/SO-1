-module(temporizador).
-export([wait/1,cronometro/3]).




wait(N) ->
    
    receive

        after

            N -> ok

    end.

cronometro(_,Hasta,_) when Hasta < 0 -> ok;  
cronometro(Fun,Hasta,Periodo) -> 
    Fun(),
    wait(Periodo),
    cronometro(Fun,Hasta-Periodo,Periodo),
    ok.




