-module(remoteChatServer).

-define(Puerto, 1236).
-define(MaxClients,25).

-export([abrirChat/0,cerrarChat/1]).


-export([recepcionista/1,mozo/2]).

-export([clientes/2]).
-export([scheduler/1]).


%% abrirChat: Crear un socket, y ponerse a escuchar.
abrirChat()->

    %% register(servidor, self()), binary,
    case gen_tcp:listen(?Puerto, [binary, {active, false},{backlog, ?MaxClients}]) of
    
        {ok, Socket} -> io:format("[Server] El chat ha abierto sus puertas\n"),
                        register(scheduler,spawn(?MODULE, scheduler,[Socket])),
                        register(clientes,spawn(?MODULE, clientes,[[],maps:new()])),
                        register(recepcionistaID,spawn(?MODULE, recepcionista, [Socket]));
                   
        {error, Reason} -> io:format("[SERVER] Ocurrio un error al intentar 
                                    escuchar ~p por la razon : ~p",[?Puerto,Reason])      
    end.


%
%% cerrarChat: Cierra el chat
cerrarChat(_algo) ->
    scheduler ! {cerrar,socket},
    ok.


%
%% recepcionista: Espera a los clientes y crea nuevos
%%                actores para atender los pedidos.
recepcionista(Socket) ->
    
        case gen_tcp:accept(Socket) of

            {ok, ClientSocket}  ->
                spawn(?MODULE, mozo,[ClientSocket,sinNickname]),
                recepcionista(Socket);
            
            {error, closed} ->
                io:format("Se cerró el socket, nos vamos a mimir"),
                exit(normal);
            
            {error, Reason} ->
                io:format("Falló la espera del client por: ~p~n",[Reason]),
                recepcionista(Socket)
            
        end.

%
%% mozo: atiende al cliente cuando este todavia no tiene nickname
mozo(Socket,sinNickname) ->

   gen_tcp:send(Socket, "[Server] Ingrese su nickname"),

    case gen_tcp:recv(Socket, 1024) of
       {ok, Nombre} ->
           io:format("este es el nombre ingresado >> ~p~n",[Nombre]),
           %N = binary_to_term(Nombre),
           io:format("[Server] El cliente ~p esta intentando ingresar el nickname ~p ~n",[Socket,Nombre]),
           scheduler ! {ingresarNickname, Nombre,Socket,self()},
           
           receive

               nombreRegistrado->gen_tcp:send(Socket, "[Server] Su nickname ha sido otorgado con exito"),
                                 mozo(Socket,Nombre);
                   
               {errorAgregar, Error} -> gen_tcp:send(Socket, "[Server] " ++ Error),
                                        mozo(Socket,sinNickname)
           end;
           
       
       {error, closed} ->
            io:format("El cliente cerró la conexión~n"),               
            gen_tcp:close(Socket),
            exit(abnormal)
                  
   end;

   
%
%% mozo: atiende al cliente cuando este tiene nickname
mozo(Socket,Nickname)->
   
   case gen_tcp:recv(Socket, 0) of 
       {ok, Paquete} ->
           %%refinar paqeute
           %% identificar operacion  (Paquete == Operacion)
           case obtener_hasta_espacio(Paquete) of

            {"/nickname",RestoPaquete}->
                {NewNickname ,_Resto} = obtener_hasta_espacio(RestoPaquete),
                scheduler ! {cambiarNickname,NewNickname,Nickname,self()},
                
                receive
                    nombreActualizado -> mozo(Socket,NewNickname);

                    nombreViejo -> mozo(Socket,Nickname)
                end;
              

            {"/msg",RestoPaquete} -> 
                    {NicknameDestino ,Mensaje} = obtener_hasta_espacio(RestoPaquete),
                    scheduler ! {mensajePrivado,Nickname,NicknameDestino,Mensaje,self()},

                    receive
                    mesajePrivadoEnviado -> mozo(Socket,Nickname);

                    {no,se,encontro,el,nickname} -> gen_tcp:send(Socket,"El nickname ingresado no existe"),
                                                    mozo(Socket,Nickname)
                    end;

            {"/exit",_RestoPaquete} ->  scheduler ! {exit,Nickname,self()},

                    receive
                        
                    rip -> gen_tcp:send(Socket,"OK"),
                           gen_tcp:close(Socket),
                           exit(normal)
                    end;
    

            
            
            {_Mensaje1,_RestoMensaje} ->
                scheduler ! {mensaje2All,Nickname,Paquete,self()},
                receive
                    {mensaje,enviado,a,todos} -> mozo(Socket,Nickname)
                end;
               

            {[],[]} -> errorPaquete,
                       mozo(Socket,Nickname)      
        
        
           end;
        
       {error, closed} ->
           io:format("El cliente cerró la conexión~n"),
           scheduler ! {exit,Nickname,self()},

                    receive
                        
                    rip -> gen_tcp:close(Socket),
                           exit(abnormal)
                    end
   end.

%
%% clientes : El actor que ejecuta esta funcion persiste
%%            una lista de Sockets y un map de Nickname-Socket
%%            de los Clientes registrados
clientes(Sockets,Nicknames_Sockets)->

    receive
        {nuevoCliente, SockClient} ->
            recepcionistaID ! {socketClient,registrado},
            clientes (Sockets ++ SockClient, Nicknames_Sockets);

        {agregar, Nombre, SockClient} -> 
            
            case maps:find(Nombre,Nicknames_Sockets) of

                {ok, _Socket}-> scheduler ! {errorAgregar,"[Server] El nombre ya esta en uso\n"},
                                clientes(Sockets,Nicknames_Sockets);

                error -> NewMap = maps:put(Nombre,SockClient,Nicknames_Sockets),
                         scheduler ! {nickname,registrado,correctamente},
                         clientes(Sockets,NewMap)
                                     
            end;

        {changeKey,NewNickname,OldNickname}->
            
            case maps:find(OldNickname,Nicknames_Sockets) of

                {ok, Socket}-> NewNicknames=maps:remove(OldNickname,Nicknames_Sockets),
                               NewMap = maps:put(NewNickname,Socket,NewNicknames),
                               scheduler ! {nickname,actualizado,correctamente},
                               clientes(Sockets,NewMap);

                error -> scheduler ! {no,se,actualizo,el,nickname},
                                    clientes(Sockets,Nicknames_Sockets)
                                     
            end ;
               
        {getSocket,NicknameDestino} -> 
            
            case maps:find(NicknameDestino,Nicknames_Sockets) of

                {ok, Socket} -> scheduler ! {socketObtenido, Socket},
                                clientes(Sockets,Nicknames_Sockets);

                error -> scheduler ! {no,se,encontro,el,nickname},
                        clientes(Sockets,Nicknames_Sockets)
            end;

        {remove, Nickname} -> NewNicknames=maps:remove(Nickname,Nicknames_Sockets),
                              scheduler ! nombreBorrado,
                              clientes(Sockets,NewNicknames);
        
        getSocketList -> scheduler ! {socketList,maps:values(Nicknames_Sockets)},
                         clientes(Sockets,Nicknames_Sockets)
            
            
    end.
%
%% scheduler : Se encarga de ejecutar los servicios
%%             que brinda el servidor
scheduler(SocketPrincipal) ->
    
    receive
        {stop,acepting} -> gen_tcp:close(SocketPrincipal)                    
    after
        0 -> receive
            {ingresarNickname, Nickname, Socket, MozoID} -> clientes ! {agregar, Nickname, Socket},
        
                receive

                    {nickname,registrado,correctamente} -> MozoID ! nombreRegistrado; 
                                                        %  Notificar a todos el nombre ingresado

                    {errorAgregar, Error} -> MozoID !  {errorAgregar, Error}
                                                    


                end,
                scheduler(SocketPrincipal);

            
            
            
            {cambiarNickname,NewNickname,OldNickname,MozoID}-> 
                
                clientes ! {changeKey,NewNickname,OldNickname},

                receive

                    {nickname,actualizado,correctamente} -> MozoID ! nombreActualizado; 
                                                        %  Notificar a todos el nombre ingresado

                    {no,se,actualizo,el,nickname} -> MozoID !  nombreViejo
                                                        %  Notificar a todos el nombre ingresado

                end,
                
                scheduler(SocketPrincipal);
            
            

            {mensajePrivado,NicknameOrigen,NicknameDestino,Msj,MozoID} -> 
                
                clientes ! {getSocket,NicknameDestino},
                
                receive

                    {socketObtenido, Socket} -> gen_tcp:send(Socket,"["++NicknameOrigen++"] "++ Msj),
                                                MozoID ! mesajePrivadoEnviado;
                                                

                    {no,se,encontro,el,nickname} -> MozoID ! nicknameNoEncontrado
                                                
                end,
            
                scheduler(SocketPrincipal);

            {mensaje2All,Nickname,Msj,MozoID} -> 
                clientes ! getSocketList,
                receive
                    {socketList,Lista} -> 
                        lists:foreach(fun (X) -> gen_tcp:send(X,"["++Nickname++"] "++ Msj) end,Lista),
                            MozoID ! {mensaje,enviado,a,todos}
                end,    
                scheduler(SocketPrincipal);
                
            {exit,Nickname,MozoID} -> clientes ! {remove,Nickname},
                                    receive
                                        nombreBorrado -> MozoID ! rip
                                    end,
                                    scheduler(SocketPrincipal)
        end                         
    end.

%
%% obtener_hasta_espacio : Recibe una string y devuelve una tupla, 
%%                         la cual en su primer componente tiene el
%%                         string hasta el primer espacio, y en la
%%                         segunda componente el resto de la string 
obtener_hasta_espacio(Paquete) ->
    lists:splitwith(fun (A) -> [A] /= " " end,Paquete).


%
%% clientes_init : Persiste una lista con los clientes 
%%                 que todavia no tienen nickname
clientes_init() ->

