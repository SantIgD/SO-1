-module(echoCliPasivo).

-define(Dir, "localhost").
-define(Puerto, 1234).
%% -define(Msj, "Holis").
-export([cliente/0]).

cliente() ->

    case gen_tcp:connect(?Dir, ?Puerto, [binary, {active, false}]) of
       
        {ok, Socket} ->

            %% io:format("Mandamos ~p~n",[?Msj]),
            %% {ok, StrIn} = io:fread("Enviar:","~s"),
            StrIn = io:get_line("Enviar:"),
            gen_tcp:send(Socket, StrIn),

            receive
                Msg -> io:format("Buzón: ~p~n",[Msg])
            
            after 1000 -> io:format("Naranja ~n")
            
            end,
            
            case gen_tcp:recv(Socket,0) of
            
                {ok , Paquete} ->
                    io:format("Nos llegó:~p~n",[Paquete]);
            
                {err, Reason }-> io:format("Error: ~p~n",[Reason])
            
            end,
            
            gen_tcp:close(Socket),
            
            ok;
        
        {error, Reason} ->
            io:format("Se produjo un error al intentar conectarse con ~p:~p: ~p~n",[?Dir,?Puerto,Reason])
    end.
