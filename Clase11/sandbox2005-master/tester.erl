-module(tester).
-export([play/0,players/1,printear/1]).
-import(mutex,[mutex_init/0,mutex_lock/1,mutex_unlock/1]).

play()->
    
    Candado=mutex_init(),
    
    P1 = spawn(?MODULE,players,[Candado]),
    P2 = spawn(?MODULE,players,[Candado]),
    P3 = spawn(?MODULE,players,[Candado]),
        
    okPlay.

players(Candado)->
    mutex_lock(Candado),
    printear(self()),
    mutex_unlock(Candado),
    okPlayers.


printear(UserPid) ->

    receive

        {UserPid,stop} -> ok;
        
        _MsgErase -> printear(UserPid) 

    after
        
        1000 -> io:format("[~p] Hello World, this is my time~n",[self()]),
                printear(UserPid)
    end.