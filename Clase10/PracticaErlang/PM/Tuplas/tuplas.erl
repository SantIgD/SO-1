-module(tuplas).
-export([play/0]).

% Función que toma un par y lo destruye en dos variables.
prim(A) ->
    {Prim, Segundo} = A,
    % Erlang soporta escribir string largos en varias lineas.
    io:format("~nLlego como argumento ~p:~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p~n",[A, Prim, Segundo]).

primQuad(B) ->
    {B1,B2,B3,B4} = B,
    io:format("~nLlego como argumento ~p:~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p ~n"
              "\t + cuya tercera es ~p ~n"
              "\t + cuya cuarta es ~p ~n",[B,B1,B2,B3,B4]).


prim2({Prim, Segundo}) ->
    % Erlang soporta escribir string largos en varias lineas.
    io:format("~nLlego como argumento una tupla:~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p~n",[Prim, Segundo]).

primQuad2({B1,B2,B3,B4}) ->
    io:format("~nLlego como argumento una tupla: ~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p ~n"
              "\t + cuya tercera es ~p ~n"
              "\t + cuya cuarta es ~p ~n",[B1,B2,B3,B4]).


primAmbas({Prim, Segundo}) ->
    % Erlang soporta escribir string largos en varias lineas.
    io:format("~nLlego como argumento una tupla:~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p~n",[Prim, Segundo]);
primAmbas({B1,B2,B3,B4}) ->
    io:format("~nLlego como argumento una tupla: ~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p ~n"
              "\t + cuya tercera es ~p ~n"
              "\t + cuya cuarta es ~p ~n",[B1,B2,B3,B4]).

play()->
    A = {jose, pepe},
    B = {1 , 2, lr, 4},
    
    prim(A),
    primQuad(B),

    prim2(A),
    primQuad2(B),

    primAmbas(A),
    primAmbas(B).
