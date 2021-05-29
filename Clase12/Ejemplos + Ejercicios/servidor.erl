-module(servidor)
.
%%%-compile([debug_info]). Para debuguear(pero es muy engorroso)

%%%%%%
%% Pequeño ejercicio de clase
%% el servidor aceptara 4 tipos de pedidos:
%% + nuevoId(Clave, PidResp) -> Se generará un nuevo identificar para `Clave` y se responderá al cliente.
%% + buscarId(Id, PidResp) -> Se responde a `PidResp` el Clave asociado a `Id`.
%% + verLista(PidResp) -> Se envía a `Pidresp` la lista de pares (Id,Clave).
%% + finalizar(PidResp) -> Se finaliza el servicio y se responde con un `ok`.


%% Creación y eliminación del servicio
-export([iniciar/0, finalizar/0]).

%% Servidor
-export([serverinit/1]).


-export([invertir/1]).
%% Librería de Acceso
-export([nuevoClave/1, quienEs/1, listaDeIds/0]).

% Iniciar crea el proceso servidor, y devuelve el PId.
iniciar() ->

    ServerID = spawn( ?MODULE
             , serverinit
             , [self()]),

    register(servidorIds,ServerID),
    ok.



%%%%%%%%%%% Servidor
%% Función de servidor de Claves.
serverinit(PInit) ->

    PInit ! ok,

    %% Comenzamos con un mapa nuevo, y un contador en 0.
    servClaves(maps:new(),0).

servClaves(Map, N) ->

    receive

        %% Llega una petición para crear un Id para Clave
        {nuevoId, Clave, ClientID} ->
            case maps:find(Clave,Map) of

                {ok, _Value}->  ClientID ! {error,"La clave ya esta en uso\n"},
                                servClaves(Map ,N);

                error ->
                        ClientID ! {claveRegistrada,Clave},
                        servClaves(maps:put(Clave,ClientID,Map),N+1)   
            end;
          
        %% Llega una petición para saber el Clave de tal Id
        {buscarId, Clave, ClientID} ->

            case maps:find(Clave,Map) of

                {ok, IdValue}->  ClientID ! {claveEncontrada,IdValue},
                                servClaves(Map ,N);

                error ->
                        ClientID ! {error,"La clave no se ha encontrado\n"},
                        servClaves(Map ,N)
            end;
                
        %% Entrega la lista completa de Ids con Claves.
        {verLista, ClientID} ->
            ListaMap = maps:to_list(Map),
            ClientID ! lists:map(fun invertir/1,ListaMap),
            servClaves(Map ,N);
    
        %% Cerramos el servidor. Va gratis
        {finalizar, ClientID} ->
            ClientID ! ok
    end.


invertir(Tupla)->
    {X,Y} = Tupla,
    {Y,X}.

%%%%%%%%%%%%%% Librería de Acceso


%% Dado un Clave y un servidor le pide que cree un identificador
%% único.
nuevoClave(Clave) ->
    servidorIds ! {nuevoId, Clave, self()},
    
    receive
        {claveRegistrada,Clave} -> io:format("Se registro la clave ~p para el proceso ~p \n",[Clave,self()]);

        {error, Error} -> io:format("[Error] ~p",[Error])
    end.

%% Función que recupera el Clave desde un Id
quienEs(Id) ->
    servidorIds ! {buscarId, Id, self()},
    receive
        {claveEncontrada,Valor} -> io:format("El valor asociado a la clave ~p es: ~p \n",[Id,Valor]);

        {error, Error} -> io:format("[Error] ~p",[Error])
    end.

%% Pedimos la lista completa de Claves e identificadores.
listaDeIds() ->
    servidorIds ! {verLista, self()},
    receive
        Lista -> io:format("[Map] ~p",[Lista])
    end.

% Ya implementada :D!
finalizar() ->
    servidorIds ! finalizar,
    unregister(servidorIds),
    ok.

