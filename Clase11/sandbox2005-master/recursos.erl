-module(recursos).
-export([alloc/2, liberar/2, allocar/3,remove_elemnt/2,tomar_recurso/2,liberar_recurso/3,cliente_exit/1,liberar_procesos/3]).

%% Función que busca un recurso libre, y lo entrega.
%% Notar que se lleva cuenta quien tiene cada recurso.
allocar([], Enuso, Pid) ->

    Pid ! noHay,
    {[], Enuso};

allocar([R1 | Recursos], Enuso, Pid) ->

    Pid ! {hay, R1},
    {Recursos, [{R1 , Pid} | Enuso]}.



liberar(Tomado,Recursos) ->

    remove_elemnt(Tomado,Recursos).

    


remove_elemnt(_Elmnt,[]) -> [];

remove_elemnt(Elmnt,[Hd|Tail])->

    if Elmnt == Hd ->
        Tail;
        true -> [Hd] ++ remove_elemnt(Elmnt,Tail)
    end.





alloc(Libres, Enuso) ->

    receive

        {tomar, Pid} ->
            {NLibres, NEnuso} = allocar(Libres, Enuso, Pid),
            io:format("[Server] New Libres = ~p // New Enuso = ~p del proceso [~p] ~n",[NLibres,NEnuso,Pid]),
            alloc(NLibres, NEnuso);

        {liberar, Pid, Recurso} ->
            NEnuso = liberar({Recurso , Pid}, Enuso),
            Pid ! {quitadoExito,Recurso},
            io:format("[Server] New Libres = ~p // New Enuso = ~p del proceso [~p] ~n",[[Recurso | Libres],NEnuso,Pid]),
            alloc([Recurso | Libres], NEnuso);

        {'EXIT',Pid, _Ra } -> 

                io:format("[Server] La razon del exit fue: ~p~n",[_Ra]),
               {NLibres,NEnuso} = liberar_procesos(Pid,Libres,Enuso),
               alloc(NLibres, NEnuso)
            
    end.

liberar_procesos(Pid,Libres,Enuso)->

    TuplaPidRecurso = search_rec_by_pid(Pid,Enuso),
            
            if TuplaPidRecurso /= vacio ->

                {Recurso , Pid} = TuplaPidRecurso ,
                NEnuso = liberar(TuplaPidRecurso, Enuso),
                io:format("[Server] New Libres = ~p // New Enuso = ~p del proceso [~p] ~n",[[Recurso | Libres],NEnuso,Pid]),
                liberar_procesos(Pid,[Recurso | Libres], NEnuso);
                

                true -> 
                    {Libres, Enuso}
            end.


tomar_recurso(ServerPid,Pid) ->

    ServerPid ! {tomar,Pid}.

liberar_recurso(ServerPid,Pid,Recurso) ->

    ServerPid ! {liberar,Pid,Recurso}.



search_rec_by_pid(_Pid,[]) -> vacio;

search_rec_by_pid(Pid,[Hd|Tail]) ->

    Response = check_in_tuple(Pid,Hd),
    
    if  Response /= no ->

        Response;

        true -> search_rec_by_pid(Pid,Tail)
    end.


cliente_exit(Msg) ->

    exit(Msg).

check_in_tuple(Pid,{Rec,PidList}) ->

    if Pid == PidList ->

       {Rec,Pid};
        
        true -> no
    end.

