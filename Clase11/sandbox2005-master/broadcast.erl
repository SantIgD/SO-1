-module(broadcast).
%% Librería
% Funciones control
-export([start/0,stop/0]).
% Funciones interacción
-export([broadcast/1,registrar/0]).
%%%
-export([loopBroadcast/2]).

start() ->
 BroadcastID = spawn(?MODULE, loopBroadcast,[[], self()]),
 register(broadcast, BroadcastID),
 ack().

ack() ->

 receive
     servOk ->
         ok;
     servErr ->
         err
  end.

stop() ->
 broadcast ! {fin, self()},
 ack().

broadcast(Msg) ->
 broadcast ! {env, self(), Msg},
 ack().

registrar() ->
 broadcast ! {reg, self()},
 ack().

%%%%%%%%%%
%%
loopBroadcast(St) ->
    receive
        {fin, Pid } -> Pid ! servOk, ok;
        {env, Pid, Msg} ->
            lists:foreach( fun (X) -> X ! Msg end , St),
            Pid ! servOk,
            loopBroadcast(St);
        {reg, Pid} ->
            Pid ! servOk,
            loopBroadcast([ Pid | St])
    end.

% Para hacer el acckerman
loopBroadcast(St, Pid) ->
    Pid ! servOk,
    loopBroadcast(St).
