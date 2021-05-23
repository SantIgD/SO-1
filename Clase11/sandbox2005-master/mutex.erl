-module(mutex).
-export([mutex_lock/1,mutex_unlock/1, mutex_init/0,lock/0]).

mutex_init() -> 

    spawn(?MODULE,lock,[]).

lock() ->
    
    receive 
        {lock,UserPid} -> 
            
            UserPid ! {self(),lockTomado},
            lock_tomado(UserPid)

    end.

lock_tomado(UserPid) ->

    receive
                {unlock,PID} ->
                    
                    if
                       
                       PID == UserPid ->  
                           UserPid !  {self(),lockSoltado},                 
                           lock();
                       
                       true ->
                           UserPid !  {self(),unlockImposible}, 
                           lock_tomado(UserPid)
                        
                    end
                
    end.

mutex_lock(CandadoPID) ->

    CandadoPID ! {lock,self()},
    
    receive 
        {CandadoPID,lockTomado} -> io:format("El proceso ~p tomo el candado ~p~n",[self(),CandadoPID])
    end.

mutex_unlock(CandadoPID) ->
   
    CandadoPID ! {unlock,self()},
    
    receive 
        {CandadoPID,lockSoltado} -> io:format("El proceso ~p solto el candado ~p~n",[self(),CandadoPID]);
        
        {CandadoPID,unlockImposible} -> err
    end.

%%mutex_unlock(CandadoPID,Value)
