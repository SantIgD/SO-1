-module(mailbox).
-export([sleep/1]).
-export([proceso_prioridad/0]).
-export([empty_mailbox/0,play/0]).
%%

%% Función Sleep
sleep(N) ->
    receive
        %%
    after
        N ->
            ok 
    end.


play() ->

    PidProcesoPrioridad = spawn(?MODULE,proceso_prioridad,[]),
    PidProcesoPrioridad ! msg2,
    PidProcesoPrioridad ! msg1,
    PidProcesoPrioridad ! msg2,
    
    ok.

%%i%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dados dos mensajes tales que msg1 tiene más prioridad que msg2
%%
%% -> msg1 -> msg2
%% -> msg2 -> msg1
%% ---
%% Cuerpo1

proceso_prioridad() ->
    sleep(1000),
    % Primero intenta hacer pattern matching con el msg1
    receive
        msg1 ->
            cuerpo1(),
            proceso_prioridad()
    after
        % en el caso de que falle intenta con msg2.
        0 -> receive
                 msg2 ->
                     cuerpo2(),
                     proceso_prioridad()
             after 0 -> io:format("no_msg~n") 
             end
    end.

cuerpo1() ->
    io:format("accion1 ~n").
cuerpo2() ->
    io:format("accion2 ~n").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


black_hole() ->
    receive
        _Todo ->
            black_hole()
    end.


empty_mailbox() ->
    receive
        _MsgVelho ->  empty_mailbox()
    after 0 -> ok
    end.


empty_until3() ->
    receive
        hasta_aca ->
            ok;
        _Msg -> empty_until3()
    end.

empty_until2() ->
    receive
       A ->
            if
                A /= hasta_aca ->
                    empty_until2();
                true ->
                    ok
            end
    %% after 0 -> ok
    end.

empty_until() ->
    self() ! hasta_aca,
    empty_until2().

