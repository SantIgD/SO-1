-module(records).


%% Persona = {nombre, edad, apellido}.
%%
-record(persona, {nombre, edad = 1, apellido = "Perez"}).

-export([martin/0,setF/1,cumplefeliz/1, personapp/1]).
-export([play/0]).


martin() ->
    #persona{ nombre = "Martín"
            , edad = 31
            , apellido = "Ceresa"}.
    %% {persona, "Martín", 31 , "Ceresa"}.


lucas() ->
    #persona{ nombre = "Lucas"
            , edad = 21
            , apellido = "Cavagna"}.
    %% {persona, "Martín", 31 , "Ceresa"}.

setF(P) ->
    P#persona{edad = 40}.

cumplefeliz(#persona{edad = Edad,apellido = Apellido} = P) ->
    P#persona{edad = Edad + 1,apellido = Apellido++"jajaja"}.

cumplefeliz2(P) ->
    P#persona{edad = P#persona.edad+1 + 1,apellido = P#persona.apellido ++"jajaja"}.

personapp(Persona) ->
    io:format("Nombre: ~p, Edad: ~p, Apellido: ~p ~n"
             ,[ Persona#persona.nombre
              , Persona#persona.edad
              , Persona#persona.apellido]).

play() ->
    personapp(martin()),
    OMartin = cumplefeliz(martin()),
    personapp(OMartin),
    personapp(lucas()),
    OLucas = cumplefeliz(martin()),
    personapp(OLucas),
    
    OMartin.
