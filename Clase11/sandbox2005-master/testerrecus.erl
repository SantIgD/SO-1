-module(testerrecus).
-export([start/0,server_init/0,client_init/2,client/2]).
-import(recursos,[alloc/2,tomar_recurso/2,liberar_recurso/3,cliente_exit/1]).


start() -> 
    
    ServerID = spawn(?MODULE,server_init,[]),
    C1 = spawn(?MODULE,client_init,[ServerID,[]]),
    C2 = spawn(?MODULE,client_init,[ServerID,[]]),
    C3 = spawn(?MODULE,client_init,[ServerID,[]]),
    
    io:format("[Cliente 1] ~p~n[Cliente 2] ~p~n[Cliente 3] ~p~n",[C1,C2,C3]),
    ok.



server_init() ->
    process_flag(trap_exit,true),
    alloc([r1,r2,r3],[]).

client_init(ServerID,Recursos) ->
    link(ServerID),
    client(ServerID,Recursos),
    ok.

client(ServerID,Recursos) ->

    receive 

        {exit,Msg} -> io:format("[Client] El cliente ~p se va a desconectar~n",[self()]),
                      cliente_exit(Msg);
        
        tomar -> tomar_recurso(ServerID,self()),
                 client(ServerID,Recursos);

        {liberar,Recurso} -> liberar_recurso(ServerID ,self(),Recurso),
                             client(ServerID,Recursos);

        noHay -> io:format("[Client] Ya no quedan recursos disponibles~n"),
                 client(ServerID,Recursos);

        {hay,Recurso} -> io:format("[Client] Se ha otorgado el recurso ~p al cliente ~p~n",[Recurso,self()]),
                          client(ServerID,[Recurso|Recursos]);

        
        {quitadoExito,Recurso} -> io:format("[Client] Se ha liberado el recurso ~p del cliente ~p~n",[Recurso,self()]),
                                  io:format("[Client] Recurso:~p~n",[Recurso]),
                                  NRecursos = remove_elemnt(Recurso,Recursos),
                                  client(ServerID,NRecursos)

    end.



remove_elemnt(_Elmnt,[]) -> [];

remove_elemnt(Elmnt,[Hd|Tail])->

    if Elmnt == Hd ->
        Tail;
        true -> [Hd] ++ remove_elemnt(Elmnt,Tail)
    end.

