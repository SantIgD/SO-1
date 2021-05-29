-module(remoteChatServer).

-define(Puerto, 1234).
-define(MaxClients,25).

-export([abrirChat/0,fin/1]).

%Librerias de acceso
-export([recepcionista/1,mozo/2]).

-export([nicknames/1]).



%% abrirChat: Crear un socket, y ponerse a escuchar.
abrirChat()->

    %% register(servidor, self()),
    case gen_tcp:listen(?Puerto, [ binary, {active, false},{backlog, ?MaxClients}]) of
    
        {ok, Socket} -> io:format("El chat ha abierto sus puertas\n"),
                        register(recepcionistaID,spawn(?MODULE, recepcionista, [Socket])),
                        register(nicknames,spawn(?MODULE, nicknames,[maps:new()]));

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

mozo(Socket,conNickname)->
   
   case gen_tcp:recv(Socket, 0) of
       
       {ok, Paquete} ->
          
           io:format("Me llegó ~p ~n",[Paquete]),
           gen_tcp:send(Socket, Paquete),
           mozo(Socket,conNickname);
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")
   end;


mozo(Socket,sinNickname) ->
   gen_tcp:send(Socket, <<"Ingrese su nickname >> ">>),

    case gen_tcp:recv(Socket, 0) of
       
       {ok, Nombre} ->
          
           io:format("[Server] El cliente ~p esta intentando ingresar el nickname ~p ~n",[Socket,Paquete]),
           nicknames ! {agregar, Nombre}
           mozo(Socket,conNickname);
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")
   end,

   mozo(Socket,conNickname).




nicknames(Nicknames) ->

    
    receive

        {agregar, Nombre, SockClient, ID} -> 

            case maps:find(Nombre,Nicknames) of

                {ok, _Socket}-> ID ! {error,"[Server] El nombre ya esta en uso\n"},
                                nicknames(Nicknames);

                error -> NewMap=maps:put(Nombre,SockClient,Nicknames),
                        ID ! {nickname,registrado,correctamente},
                        nicknames(NewMap)
                                     
            end
        
    end.
                
    



