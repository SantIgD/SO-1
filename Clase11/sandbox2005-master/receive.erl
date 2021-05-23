-module(receive).
-export([sleep/1]).
%% proceso_prioridad/0]).

%% Función Sleep
sleep(N) ->
    receive
        %%
    after
        N ->
            ok
    end.
