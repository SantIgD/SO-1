-module(testersem).
-export([play/0,players/1,printear/2]).
-import(semaphore,[sem_init/1,sem_post/1,sem_wait/1,sem_get_value/1,sem_destroy/1]).

play()->
    
    Semaforo=sem_init(1),
    
    P1 = spawn(?MODULE,players,[Semaforo]),
    P2 = spawn(?MODULE,players,[Semaforo]),
    P3 = spawn(?MODULE,players,[Semaforo]),
        
    receive 
    after 
        17000 ->sem_destroy(Semaforo)
    end,


    okPlay.

players(Semaforo)->
    sem_wait(Semaforo),
    printear(self(),5),
    sem_get_value(Semaforo),
    sem_post(Semaforo).
    %%sem_get_value(Semaforo).
    

printear(_UserPid,0) -> ok;

printear(UserPid,N) ->
    receive
    after
        
        1000 -> io:format("[~p] Hello World, this is my time~n",[self()]),
                printear(UserPid,N-1)
    end.