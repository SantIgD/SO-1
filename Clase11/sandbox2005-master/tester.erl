-module(tester).
-export([play/0,players/1,printear/2]).
-import(mutex,[mutex_init/0,mutex_lock/1,mutex_unlock/1]).

play()->
    
    Candado=mutex_init(),
    
    P1 = spawn(?MODULE,players,[Candado]),
    P2 = spawn(?MODULE,players,[Candado]),
    P3 = spawn(?MODULE,players,[Candado]),
        
    okPlay.

players(Candado)->
    mutex_lock(Candado),
    printear(self(),5),
    mutex_unlock(Candado),
    okPlayers.

printear(UserPid,0) -> ok;

printear(UserPid,N) ->
    receive
    after
        
        1000 -> io:format("[~p] Hello World, this is my time~n",[self()]),
                printear(UserPid,N-1)
    end.