-module(remoteChatServer).

-define(Puerto, 1262).
-define(MaxClients,25).

-export([abrirChat/0,cerrarChat/0]).
-export([showNicknames/0]).
-export([recepcionista/1,mozo/2]).
-export([clientes/2]).
-export([scheduler/2]).


%% abrirChat: Crear un socket, y ponerse a escuchar.
abrirChat()->

    %% register(servidor, self()), binary,
    case gen_tcp:listen(?Puerto, [binary, {active, false},{backlog, ?MaxClients}]) of
    
        {ok, Socket} -> io:format("[ChatServer] El chat ha abierto sus puertas\n"),
                        SchedulerID = spawn(?MODULE, scheduler,[Socket]),
                        register(scheduler,SchedulerID),
                        register(clientes,spawn(?MODULE, clientes,[[],maps:new()])),
                        register(recepcionistaID,spawn(?MODULE, recepcionista, [Socket]));
                   
        {error, Reason} -> io:format("[SERVER] Ocurrio un error al intentar 
                                    escuchar ~p por la razon : ~p",[?Puerto,Reason])      
    end.


%
%% cerrarChat: Cierra el chat
cerrarChat() ->
    recepcionistaID ! rip,
    scheduler ! stopAccepting,
    ok.


%
%% recepcionista: Espera a los clientes y crea nuevos
%%                actores para atender los pedidos.
recepcionista(Socket) ->
    
        case gen_tcp:accept(Socket,500) of

            {ok, ClientSocket}  ->
                clientes ! {nuevoCliente, ClientSocket},

                receive 
                    {socketClient,registrado} -> io:format("[ChatServer] Un cliente salvaje ha aparecido!\n");

                    {error,Error} -> io:format("[ChatServer] No se ha podido registrar el cliente, error: ~p \n",[Error])
                end,

                spawn(?MODULE, mozo,[ClientSocket,sinNickname]),
                recepcionista(Socket);
            
            {error, closed} ->
                io:format("[ChatServer] Se cerró el socket, nos vamos a mimir");
               
                
            {error, _Reason} ->
                Response = getServerState(),
                if  Response == {closed,empty} -> exit(normal);
                true -> recepcionista(Socket)
                end

            
        end.

%
%% mozo: atiende al cliente cuando este todavia no tiene nickname
mozo(Socket,sinNickname) ->

   gen_tcp:send(Socket, "[ChatServer] Ingrese su nickname"),

    case gen_tcp:recv(Socket, 1024) of
       {ok, Nombre} ->
           N = bigbinary_to_term(Nombre),

           if 
               N == "/exit" -> 
                   
                   gen_tcp:send(Socket,"OK"),
                   gen_tcp:close(Socket),
                   exit(normal);

               true -> ok
            end,
                
           %N = binary_to_term(Nombre),
           %io:format("este es el nombre ingresado >> ~p~n",[N]),
           %io:format("[ChatServer] El cliente ~p esta intentando ingresar el nickname ~p ~n",[Socket,Nombre]),
           scheduler ! {ingresarNickname, N,Socket,self()},
           
           receive

               nombreRegistrado->gen_tcp:send(Socket, "[ChatServer] Bienvenido al Chat ["++N++"]"),
                                 mozo(Socket,N);
                   
               {errorAgregar, Error} -> gen_tcp:send(Socket, "[ChatServer] " ++ Error),
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
       {ok, N} ->
           Paquete = bigbinary_to_term(N),
           %io:format("Paquete = ~p ~n",[Paquete]),
           case obtener_hasta_espacio(Paquete) of

            {"/nickname",RestoPaquete}->
                NewNickname= lists:subtract(RestoPaquete," "),
                scheduler ! {cambiarNickname,NewNickname,Nickname,self()},
                
                receive
                    nombreActualizado -> gen_tcp:send(Socket, "[ChatServer] Has cambiado tu nombre a ["++NewNickname++"]"),
                                        mozo(Socket,NewNickname);

                    nombreViejo -> gen_tcp:send(Socket, "[ChatServer] No se ha podido cambiar al nickname"),
                                    mozo(Socket,Nickname)
                end;
              

            {"/msg",RestoPack} -> 
                    RestoPaquete= lists:subtract(RestoPack," "),
                    {NicknameDestino ,Msj} = obtener_hasta_espacio(RestoPaquete),
                    Mensaje = lists:subtract(Msj," "),
                    %io:format("Nickname de origen = ~p ,Nickname de dest = ~p , mensaje = ~p~n",[Nickname,NicknameDestino,Mensaje]),
                    scheduler ! {mensajePrivado,Nickname,NicknameDestino,Mensaje,self()},

                    receive
                    mesajePrivadoEnviado -> mozo(Socket,Nickname);

                    nicknameNoEncontrado -> gen_tcp:send(Socket,"El nickname ingresado no existe"),
                                                    mozo(Socket,Nickname)
                    end;

            {"/exit",_RestoPaquete} ->  scheduler ! {closeClient,Nickname,self()},

                    receive
                        
                    rip -> gen_tcp:send(Socket,"OK"),
                           gen_tcp:close(Socket),
                           exit(normal)
                    end;
    

            
            {[],[]} -> errorPaquete,
                       mozo(Socket,Nickname);

            {_Mensaje1,_RestoMensaje} ->
                scheduler ! {mensaje2All,Nickname,Paquete,self()},
                receive
                    {mensaje,enviado,a,todos} -> mozo(Socket,Nickname)
                end
        
           end;
        
       {error, closed} ->
           io:format("[ChatServer] El cliente cerró la conexión~n"),
           gen_tcp:send(Socket,"OK"),
           gen_tcp:close(Socket),
           exit(normal)
   end.


%
%% clientes : El actor que ejecuta esta funcion persiste
%%            una lista de Sockets y un map de Nickname-Socket
%%            de los Clientes registrados
clientes(Sockets,Nicknames_Sockets)->
       
    receive
        stopAccepting-> unregister(clientes),
                        exit(normal)
     after 
            0->receive
                    
                    %%para el exit de mozo sin nombre
                    {eliminarSocket ,Socket} -> clientes(lists:delete(Socket,Sockets),Nicknames_Sockets);
                    {nuevoCliente, SockClient} ->
                        recepcionistaID ! {socketClient,registrado},
                        clientes (Sockets ++ [SockClient], Nicknames_Sockets);

                    {agregar, Nombre, SockClient} -> 
                        
                        case maps:find(Nombre,Nicknames_Sockets) of

                            {ok, _Socket}-> 
                                           scheduler ! {errorAgregar,"[ChatServer] El nombre ya esta en uso\n"},
                                            clientes(Sockets,Nicknames_Sockets);
                                      
              
                            error ->
                                    NewMap = maps:put(Nombre,SockClient,Nicknames_Sockets),
                                     scheduler ! {nickname,registrado,correctamente},
                                     %io:format("Lista de sockets ~p~n",[Sockets]),
                                     clientes(lists:delete(SockClient,Sockets),NewMap)
                                                 
                        end;

                    {changeKey,NewNickname,OldNickname}->
                        
                        case maps:find(NewNickname,Nicknames_Sockets) of

                            {ok, _Socket}-> scheduler ! {no,se,actualizo,el,nickname},
                                           clientes(Sockets,Nicknames_Sockets);

                            error ->  {ok, SocketOld}=maps:find(OldNickname,Nicknames_Sockets),
                                      NewNicknames=maps:remove(OldNickname,Nicknames_Sockets),
                                      NewMap = maps:put(NewNickname,SocketOld,NewNicknames),
                                      scheduler ! {nickname,actualizado,correctamente},
                                      clientes(Sockets,NewMap)
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
                    
                    getNamedSocketsList -> scheduler ! {namedSocketsList,maps:values(Nicknames_Sockets)},
                                     clientes(Sockets,Nicknames_Sockets);

                    getSocketsList -> scheduler ! {socketsList,maps:values(Nicknames_Sockets)++Sockets},
                                     clientes(Sockets,Nicknames_Sockets);

                    {getNicknamesList, ID} -> ID ! {allNicknamesList,maps:keys(Nicknames_Sockets)} 
                                            after
                                                1000 -> clientes(Sockets,Nicknames_Sockets)                   
                                            end          
            
    end.



%
%% scheduler : Se encarga de ejecutar los servicios
%%             que brinda el servidor
scheduler(SocketPrincipal,State) when State == open->
    
    receive
        stopAccepting -> 
                           %gen_tcp:close(SocketPrincipal)
                           %%gen_tcp:close(SocketPrincipal),
                           %%unregister(recepcionistaID),
                           clientes ! getSocketList,
                           
                                receive
                                   {socketsList,Lista} -> 
                                   lists:foreach(fun (X) -> gen_tcp:send(X,"shutdown") end,Lista)   
                                end                    
    after
        0 -> receive
                {ingresarNickname, Nickname, Socket, MozoID} -> clientes ! {agregar, Nickname, Socket},
            
                    receive

                        {nickname,registrado,correctamente} -> MozoID ! nombreRegistrado; 
                                                            %  Notificar a todos el nombre ingresado

                        {errorAgregar, Error} -> MozoID !  {errorAgregar, Error}
                                                      


                    end,
                    scheduler(SocketPrincipal,open);

                
                
                
                {cambiarNickname,NewNickname,OldNickname,MozoID}-> 
                    
                    clientes ! {changeKey,NewNickname,OldNickname},

                    receive

                        {nickname,actualizado,correctamente} -> MozoID ! nombreActualizado; 
                                                            %  Notificar a todos el nombre ingresado

                        {no,se,actualizo,el,nickname} -> MozoID !  nombreViejo
                                                            %  Notificar a todos el nombre ingresado

                    end,
                    
                    scheduler(SocketPrincipal,open);
                
                

                {mensajePrivado,NicknameOrigen,NicknameDestino,Msj,MozoID} -> 
                    
                    clientes ! {getSocket,NicknameDestino},
                    
                    receive

                        {socketObtenido, Socket} -> gen_tcp:send(Socket,"["++NicknameOrigen++"] "++ Msj),
                                                    MozoID ! mesajePrivadoEnviado;
                                                    

                        {no,se,encontro,el,nickname} -> MozoID ! nicknameNoEncontrado
                                                    
                    end,
                
                    scheduler(SocketPrincipal,open);

                {mensaje2All,Nickname,Msj,MozoID} -> 
                    clientes ! getNamedSocketsList,
                    receive
                        {namedSocketsList,Lista} -> 
                            lists:foreach(fun (X) -> gen_tcp:send(X,"["++Nickname++"] "++ Msj) end,Lista),
                                MozoID ! {mensaje,enviado,a,todos}
                    end,    
                    scheduler(SocketPrincipal,open);
                    
                {closeClient,Nickname,MozoID} -> clientes ! {remove,Nickname},
                                        receive
                                            nombreBorrado -> MozoID ! rip
                                        end,
                                        scheduler(SocketPrincipal,open);
                {getState,ID} -> ID ! State
             after
                 1000 -> scheduler(SocketPrincipal,open)                     
             end                         
    end;

scheduler(SocketPrincipal,State) when State == closed->
    
    clientes ! getNamedSocketsList,

    receive 
        {namedSocketsList,List} ->
            if (List == []) -> exit(normal);
            true -> ok % Faltan eliminar clientes
            end
    end,

    receive

        {closeClient,Nickname,MozoID} -> clientes ! {remove,Nickname},
                                        receive
                                            nombreBorrado -> MozoID ! rip
                                        end,
                                        scheduler(SocketPrincipal,closed);

        {getState,ID} -> ID ! {schedulerState,State}
    end.


%
%% showNicknames : Muestra los nicknames registrados en el momento de su llamada
showNicknames() ->
    clientes ! {getNicknamesList,self()},
                    receive
                        {allNicknamesList,Lista} -> 
                        io:format("[ChatServer] Nicknames: ~p\n",[Lista])
                    end.    
                    

%
%% obtener_hasta_espacio : Recibe una string y devuelve una tupla, 
%%                         la cual en su primer componente tiene el
%%                         string hasta el primer espacio, y en la
%%                         segunda componente el resto de la string 
obtener_hasta_espacio(Paquete) ->
   lists:splitwith(fun (A) -> [A] /= " " end,Paquete).

%
%% bibinary_to_term : Recibe un binario, agregrega el sufijo string
%%                    en binario de Erlang y retorna lo que significa
%%                    en string
bigbinary_to_term(Nombre)->
    Lista = binary_to_list(Nombre),
    {ListaProcesada, Size} = get_up_to_0(Lista,[],0),
    binary_to_term(list_to_binary([131,107,0,Size]++ListaProcesada)).

%
%% get_up_to_0 :   Recibe una lista de valores y la recorta
%%                    al encontrar el primer 0 (valor no util del binario) 
%%                   , obteniendo tambien la cantidad de caracteres transmitidos
get_up_to_0([Hd|Tail],Resultado,ContSize) ->
    
    if Hd == 0 ->
        {Resultado,ContSize};
    
        true -> get_up_to_0(Tail,Resultado ++ [Hd],ContSize+1)
    end.




getServerState() ->

    scheduler ! {getState,self()},

    receive

        {schedulerState,State} -> 

            clientes ! getNamedSocketsList,

            receive 
                {namedSocketsList,List} ->
                    if (List == []) -> {State,empty};
                    true -> {State,nonEmpty}
                    end
            end
    end.

    


