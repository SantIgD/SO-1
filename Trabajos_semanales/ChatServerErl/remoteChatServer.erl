-module(remoteChatServer).

-define(Puerto, 1236).
-define(MaxClients,25).

-export([abrirChat/0,fin/1]).

%Librerias de acceso
-export([recepcionista/1,mozo/2]).

-export([nicknames/1]).
-export([scheduler/0]).


%% abrirChat: Crear un socket, y ponerse a escuchar.
abrirChat()->

    %% register(servidor, self()), binary,
    case gen_tcp:listen(?Puerto, [binary, {active, false},{backlog, ?MaxClients}]) of
    
        {ok, Socket} -> io:format("[Server] El chat ha abierto sus puertas\n"),
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
           io:format("El cliente cerró la conexión~n")
   end;

   

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
                        
                    rip -> gen_tcp:close(Socket),
                           exit(normal)
                    end;
    

            {[],[]} -> errorPaquete;

           {_ComandoFail,_RestoPaquete} ->
               ok
           end,
           mozo(Socket,Nickname);
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")
   end.


nicknames(Nicknames) ->

    
    receive

        {agregar, Nombre, SockClient} -> 
            
            case maps:find(Nombre,Nicknames) of

                {ok, _Socket}-> scheduler ! {errorAgregar,"[Server] El nombre ya esta en uso\n"},
                                nicknames(Nicknames);

                error -> NewMap = maps:put(Nombre,SockClient,Nicknames),
                         scheduler ! {nickname,registrado,correctamente},
                         nicknames(NewMap)
                                     
            end;

        {changeKey,NewNickname,OldNickname}->
            
            case maps:find(OldNickname,Nicknames) of

                {ok, Socket}-> NewNicknames=maps:remove(OldNickname,Nicknames),
                               NewMap = maps:put(NewNickname,Socket,NewNicknames),
                               scheduler ! {nickname,actualizado,correctamente},
                               nicknames(NewMap);

                error -> scheduler ! {no,se,actualizo,el,nickname},
                                    nicknames(Nicknames)
                                     
            end ;
               
        {getSocket,NicknameDestino} -> 
            
            case maps:find(NicknameDestino,Nicknames) of

                {ok, Socket} -> scheduler ! {socketObtenido, Socket},
                                nicknames(Nicknames);

                error -> scheduler ! {no,se,encontro,el,nickname},
                        nicknames(Nicknames)
            end;

        {remove, Nickname} -> NewNicknames=maps:remove(Nickname,Nicknames),
                              scheduler ! nombreBorrado,
                              nicknames(NewNicknames)
            
            
    end.
                
    



scheduler() ->

    receive

        {ingresarNickname, Nickname, Socket, MozoID} -> nicknames ! {agregar, Nickname, Socket},
    
            receive

                {nickname,registrado,correctamente} -> MozoID ! nombreRegistrado; 
                                                    %  Notificar a todos el nombre ingresado

                {errorAgregar, Error} -> MozoID !  {errorAgregar, Error}
                                                   


            end,
            scheduler();

        
        
        
        {cambiarNickname,NewNickname,OldNickname,MozoID}-> 
            
            nicknames ! {changeKey,NewNickname,OldNickname},

            receive

                {nickname,actualizado,correctamente} -> MozoID ! nombreActualizado; 
                                                    %  Notificar a todos el nombre ingresado

                {no,se,actualizo,el,nickname} -> MozoID !  nombreViejo
                                                    %  Notificar a todos el nombre ingresado

            end,
            
            scheduler();
        
        

        {mensajePrivado,NicknameOrigen,NicknameDestino,Msj,MozoID} -> 
            
            nicknames ! {getSocket,NicknameDestino},
            
            receive

                {socketObtenido, Socket} -> gen_tcp:send(Socket,"["++NicknameOrigen++"] "++ Msj),
                                            MozoID ! mesajePrivadoEnviado,
                                            scheduler();

                {no,se,encontro,el,nickname} -> MozoID ! nicknameNoEncontrado,
                                                scheduler()
            end,
            
            
            scheduler();
        {mensaje2All,_Nickname,_Msj} -> scheduler(),ok;
        
        {exit,Nickname,MozoID} -> nicknames ! {remove,Nickname},
                                receive
                                    nombreBorrado -> MozoID ! rip
                                end,
                                scheduler()
                                
    end.


obtener_hasta_espacio(Paquete) ->
    lists:splitwith(fun (A) -> [A] /= " " end,Paquete).




