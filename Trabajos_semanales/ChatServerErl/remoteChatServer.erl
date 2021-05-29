-module(remoteChatServer).

-define(Puerto, 1234).
-define(MaxClients,25).

-export([abrirChat/0,fin/1]).

%Librerias de acceso
-export([recepcionista/1,mozo/2]).

-export([nicknames/1]).
-export([scheduler/0]).


%% abrirChat: Crear un socket, y ponerse a escuchar.
abrirChat()->

    %% register(servidor, self()), binary,
    case gen_tcp:listen(?Puerto, [ {active, false},{backlog, ?MaxClients}]) of
    
        {ok, Socket} -> io:format("El chat ha abierto sus puertas\n"),
                        register(recepcionistaID,spawn(?MODULE, recepcionista, [Socket])),
                        register(nicknames,spawn(?MODULE, nicknames,[maps:new()])),
                        register(scheduler,spawn(?MODULE, scheduler,[]));


        {error, Reason} -> io:format("[SERVER] Ocurrio un error al intentar 
                                    escuchar ~p por la razon : ~p",[?Puerto,Reason])      
    end.



fin(Socket) ->
    gen_tcp:close(Socket),
    ok.

%% recepcionista: Espera a los clientes y crea nuevos actores para atender los pedidos.
%%
recepcionista(Socket) ->
    
    %% receive
    %%     fin -> gen_tcp:close(Socket), ok
    %% after 0 ->
    
        case gen_tcp:accept(Socket) of
            
            {ok, ClientSocket}  ->
                
                spawn(?MODULE, mozo,[ClientSocket,sinNickname]);
            
            {error, closed} ->
                io:format("Se cerró el socket, nos vamos a mimir"),
                exit(normal);
            
            {error, Reason} ->
                io:format("Falló la espera del client por: ~p~n",[Reason])
            
        end,
        recepcionista(Socket).
    %% end.

%% mozo: atiende al cliente.
%%


mozo(Socket,sinNickname) ->

   gen_tcp:send(Socket, "[Server] Ingrese su nickname >> "),

    case gen_tcp:recv(Socket, 0) of
       
       {ok, Nombre} ->
          
           io:format("[Server] El cliente ~p esta intentando ingresar el nickname ~p ~n",[Socket,Nombre]),
           
           scheduler ! {ingresarNickname, Nombre,Socket,self()},
           
           receive
               ok->mozo(Socket,Nombre),
                   ok;
               _Error -> gen_tcp:send(Socket, "[Server]" ++ _Error ++" \n"),
                         mozo(Socket,sinNickname)
           end;
           
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")
   end;

   

mozo(Socket,_Nickname)->
   
   case gen_tcp:recv(Socket, 0) of
       
       {ok, Paquete} ->
          
           io:format("Me llegó ~p ~n",[Paquete]),
           gen_tcp:send(Socket, Paquete),
           mozo(Socket,conNickname);
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")
   end.


nicknames(Nicknames) ->

    
    receive

        {agregar, Nombre, SockClient, SchedulerID} -> 
            
            case maps:find(Nombre,Nicknames) of

                {ok, _Socket}-> SchedulerID ! {error,"[Server] El nombre ya esta en uso\n"},
                                nicknames(Nicknames);

                error -> NewMap = maps:put(Nombre,SockClient,Nicknames),
                         SchedulerID ! {nickname,registrado,correctamente},
                         nicknames(NewMap)
                                     
            end
        
    end.
                
    



scheduler() ->

    receive

        {ingresarNickname, Nickname, Socket, MozoID} -> nickname ! {agregar, Nickname, Socket, self()},
    
            receive

                {nickname,registrado,correctamente} -> MozoID ! ok;

                _Error -> MozoID ! _Error

            end;

        {cambiarNickname,_NewNickname,_OldNickname}-> scheduler(),ok;
        
        {mensaje2All,_Nickname,_Msj} -> scheduler(),ok;

        {mensajePrivado,_NicknameOrigen,_NicknameDestino,_Msj} -> scheduler(),ok;

        {exit,_Nickname} -> scheduler(),ok
    end.

