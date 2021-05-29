-module(bintree).

-record(nodo,{child1 = {nodo}, child2 = {nodo}, valor = sinValor}).


-record(nodo2,{child1 = {nodo}, child2 = {nodo}, valor = sinValor, estado = asignarValor}).


-export([iniciar/0,arbol_init/0]).

-export([bin/0]).

%%arbol a representar
%%                    50
%%           30                90
%%      20        40                 100

iniciar() ->

    register(arbolID,spawn(?MODULE,arbol_init,0)),
    ok.


bin()->
    N20  = #nodo{child1 = null, child2 = null, valor = 20},
    N40  = #nodo{child1 = null, child2 = null, valor = 40},
    N100 = #nodo{child1 = null, child2 = null, valor = 100},
    N30  = #nodo{child1 = N20, child2 = N40, valor = 30},
    N90  = #nodo{child1 = null, child2 = N100, valor = 90},
    N50  = #nodo{child1 = N30, child2 = N90, valor = 50},
    printear(N50).

printear(null)-> null;

printear(P)->
    io:format("~p/~p/~p~n",[printear(P#nodo.child1),P#nodo.valor,printear(P#nodo.child2)]).

arbol_init() ->
    
    arbol( #nodo2{child1 = {nodo}, child2 = {nodo}, valor = sinValor}),
    ok.

%%#persona{edad = Edad,apellido = Apellido} = P

arbol(#nodo2{estado = Estado} = Arbol) ->

     receive

        {agregarNodo,Valor} -> 

            if Estado == asignarValor -> 
                
                Arbol#nodo2{child1 = {nodo},child2 = {nodo}, valor = Valor,estado = asignarHijoIzquierdo},
                arbol(Arbol);

            
            Estado == asignarHijoIzquierdo  ->

                Arbol#nodo2{child1 = crear_nodo(Valor), estado = asginarHijoDerecho},
                arbol(Arbol);

            Estado == asginarHijoDerecho ->

                
                Arbol#nodo2{child2 = crear_nodo(Valor), estado = lleno},
                arbol(Arbol#nodo.child1,Arbol);

            true -> error
            
            end
    end,        
    ok.

arbol(#nodo2{estado = Estado} = Arbol, RaizArbol) ->
    
    receive

        {agregarNodo,Valor} -> 

            if Estado == asignarValor -> 
                
                Arbol#nodo2{child1 = {nodo},child2 = {nodo}, valor = Valor,estado = asignarHijoIzquierdo},
                arbol(Arbol);

            
            Estado == asignarHijoIzquierdo  ->

                Arbol#nodo2{child1 = crear_nodo(Valor), estado = asginarHijoDerecho},
                arbol(Arbol);

            Estado == asginarHijoDerecho ->

                Arbol#nodo2{child2 = crear_nodo(Valor), estado = lleno},
                arbol(Arbol#nodo.child1,RaizArbol);

            true -> error
            
            end
    end,        
    ok.



crear_nodo(Valor) ->
    
    #nodo2{child1 = {nodo}, child2 = {nodo}, valor = Valor}.

agregar_valor(Valor) ->

    arbolID ! {agregarNodo, Valor}.

print_arbol(Arbol) ->

    arbolID ! print


