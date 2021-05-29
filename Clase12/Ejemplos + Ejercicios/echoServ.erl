-module(echoServ).
-define(Puerto, 1234).
-export([start/0,fin/1,receptor/1,echoResp/1]).

%% start: Crear un socket, y ponerse a escuchar.

start()->
    %% register(servidor, self()),
    {ok, Socket} = gen_tcp:listen(?Puerto
                                 , [ binary, {active, false}]),
    spawn(?MODULE,receptor, [Socket]),
    Socket .

fin(Socket) ->
    gen_tcp:close(Socket),
    ok.

%% receptor: Espera a los clientes y crea nuevos actores para atender los pedidos.
%%
receptor(Socket) ->
    %% receive
    %%     fin -> gen_tcp:close(Socket), ok
    %% after 0 ->
        case gen_tcp:accept(Socket) of
            
            {ok, CSocket}  ->
                spawn(?MODULE, echoResp,[CSocket]);
            
            {error, closed} ->
                io:format("Se cerró el closed, nos vamos a mimir"),
                exit(normal);
            
            {error, Reason} ->
                io:format("Falló la espera del client por: ~p~n",[Reason])
        end,
        receptor(Socket).
    %% end.

%% echoResp: atiende al cliente.
%%
echoResp(Socket) ->
   
   case gen_tcp:recv(Socket, 0) of
       
       {ok, Paquete} ->
          
           io:format("Me llegó ~p ~n",[Paquete]),
           gen_tcp:send(Socket, Paquete),
           echoResp(Socket);
       
       {error, closed} ->
           io:format("El cliente cerró la conexión~n")
   end.
