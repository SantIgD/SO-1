-module(ms).
-export([start/1,crear_servicios/2,supervisor/2,supervisor_init/0,servicio_init/1,servicio/0,to_service/2,search/2]).
-export([search_pid/3,change_element/4]).



start(N) when N > 0 ->
    Pid = spawn(?MODULE,supervisor_init,[]),
    register(supervisor,Pid),
    crear_servicios(N,Pid),
    ok.
    
crear_servicios(0,_Pid) -> 
    ok;
crear_servicios(N,Pid) ->

    spawn(?MODULE,servicio_init,[Pid]),
    io:format("Se creo el servicio numero ~p ~n",[N]),
    crear_servicios(N-1,Pid),
    ok.

supervisor(St,M)->

    receive

        {reg,Pid}->

            Pid ! {supervisor,registrado},
            supervisor([Pid | St],M+1);

        {Msg,N}->
            
            ServPid = search(St,M-N),
            ServPid ! {supervisor,Msg},
            supervisor(St,M);

        {'EXIT',Pid, _Ra } -> 
               Indice = search_pid(St,Pid,M),
               NewPid = spawn(?MODULE,servicio_init,[self()]),
               change_element(St,NewPid,Indice,M),
               io:format("Se creo el servicio numero ~p ~n",[Indice]),
               supervisor(St,M)

            
    end.    

supervisor_init() ->

    process_flag(trap_exit,true),
    supervisor([],0),
    ok.


servicio_init(SupPid)->
    link(SupPid),
    supervisor ! {reg , self()},
    receive 

        {supervisor,registrado} -> io:format("[~p] registrado en supervisor~n",[self()])   

    end,

    servicio().


servicio() ->
    
    receive
        
        
        {supervisor, die} ->io:format("[~p] Ha sido asesinado ~n",[self()]), 
                            exit(die);

        {supervisor, Msg} -> io:format("~p~n",[Msg]),
                            servicio()
    end.   


to_service(Msg,N) ->
    
    supervisor ! {Msg,N},
    ok.

search([Hd|_Tail],0) -> Hd;

search([_Hd | St],N) -> 
    search(St,N-1).
    

search_pid(_St,_Pid,0) -> err;

search_pid([Hd|Tail],Pid,M) ->

    io:format("[~p] vs [~p] ~n",[Hd,Pid]),

    if Hd == Pid ->
         M;
        true -> 
            search_pid(Tail,Pid,M-1)
    end.


change_element(_List,_NewPid,_Indice,0) -> err;
change_element([Hd|Tail],NewPid,Indice,M)->

    if Indice == M -> [NewPid|Tail];

       true -> [Hd] ++ change_element(Tail,NewPid,Indice,M-1)

    end.
    
    
