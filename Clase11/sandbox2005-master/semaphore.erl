-module(semaphore).
-export([sem_init/1,sem_post/1,sem_wait/1,semaforo/1,sem_get_value/1]).


sem_init(Cantidad)->
    spawn(?MODULE,semaforo,[Cantidad]).



semaforo(0) ->

    receive
    
        {add} -> semaforo(1);

        {show,Pid} -> io:format("[Semaforo ~p] [Value] ~p~n",[self(),0]),
                      Pid ! {cantidad,0},
                      semaforo(0)
    
    end;

semaforo(Cantidad) ->

        
    receive 
    
        {add} -> semaforo(Cantidad + 1);

        {rest,Pid} -> 
            Pid ! {pasar,Pid},
            semaforo(Cantidad -1);

        {show,Pid} -> io:format("[Semaforo ~p] [Value] ~p~n",[self(),Cantidad]),
                      Pid ! {cantidad,Cantidad},
                      semaforo(Cantidad)
    
    end. 



sem_post(Semaforo)->
    Semaforo ! {add}.


sem_wait(Semaforo)->
    
    MyPid = self(),
    Semaforo ! {rest,self()},
    receive
        {pasar,MyPid} -> ok
    end.


sem_get_value(Semaforo)-> 


    Semaforo ! {show,self()},
    receive
        {cantidad,Cantidad} -> Cantidad
    end.
    
    
            