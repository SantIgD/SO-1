# Pattern Matching

En Erlang el *Pattern Matching* es usado para realizar asignación de valores y
para controlar el flujo del programa. La idea principal es que, como en la
mayoría de los lenguajes declarativos, la asignación en realidad declara una
equivalencia entre dos términos.

De la misma manera que en matemáticas escribimos:
```math
x + 1 = 5
```
Y deducimos que `x = 4`.

Erlang va a hacer la deducción por nosotros, y en caso que no sea posible
nos va a informar que no puede hacerlo. La diferencia en concreto es que
nos permitirá hacerlo con los *Tipos de Datos* antes mencionados.


## Tuplas

En el caso de que una variable `A` sea una tupla de dos elementos, podemos
*destruirla* y obtener sus elementos con la ayuda de pattern matching.
```erlang
{Primero, Segundo} = A,
```
De esta manera podemos pensar que `Primero` y `Segundo` son variables que alojan
los valores que hacen que la ecuación `{Primero, Segundo} = A` sea verdadera.

Notar que lo mismo lo podemos hacer para tuplas de la longitud que queramos. Por
ejemplo, sea `B` una tupla de 4 elementos:
```erlang
{Primero, Segundo, Tercero, Cuarto} = B
```

Por ejemplo podemos verlo en el siguiente código, en un archivo `tuplas.erl`:
```erlang
-module(tuplas).
-export([play/0]).

% Función que toma un par y lo destruye en dos variables.
prim(A) ->
    {Prim, Segundo} = A,
    % Erlang soporta escribir string largos en varias lineas.
    io:format("Llego como argumento ~p:~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p~n",[A, Prim, Segundo]).

primQuad(B) ->
    io:format("Ejercicio?~n").

play()->
    A = {jose, pepe},
    prim(A),
    B = {1 , 2, lr, 4},
    primQuad(B).
```

Si prestamos atención a las funciones `prim` y `primQuad` resulta que para saber
*qué información* tiene un argumento de tipo tupla lo tenemos que destruir.

Para esto es útil entonces que las funciones puedan hacer pattern matching
directamente con sus argumentos.

Quedando de la siguiente forma:
```erlang
-module(tuplas).
-export([play/0]).

% Función que toma un par y lo destruye en dos variables.
prim({Prim, Segundo}) ->
    % Erlang soporta escribir string largos en varias lineas.
    io:format("Llego como argumento una tupla:~n"
              "\t + cuya primer componente es ~p ~n"
              "\t + cuya segunda es ~p~n",[Prim, Segundo]).

primQuad(B) ->
    io:format("Ejercicio?~n").

play()->
    A = {jose, pepe},
    prim(A),
    B = {1 , 2, lr, 4},
    primQuad(B).
```

Al igual que en matemáticas podemos definir una función por casos, en Erlang
podemos hacer análisis por casos dependiendo el pattern matching de sus
argumentos. Y al igual que en una función por casos, la función se comporta
dependiendo la cláusula que sea verdadera.

Por lo que podemos juntar las dos funciones mencionadas anteriormente:
```erlang
-module(tuplas).
-export([play/0]).

primAmbas({Prim, Seg}) ->
    io:format("Llego como argumento una tupla:~n"
                "\t + cuya primer componente es ~p ~n"
                "\t + cuya segunda es ~p~n",[Prim, Seg]);
primAmbas(B) ->
    io:format("Ejercicio?~n").


play()->
    A = {jose, pepe},
    primAmbas(A),
    B = {1 , 2, lr, 4},
    primAmbas(B).
```

## Listas

Las listas, a diferencia de las tuplas, son de longitud variable. Esto nos
indica que el pattern matching será distinto, ya que en general no vamos a poder
recuperar todos los valores de una lista. Lo bueno es que tampoco deberíamos
querer hacerlo.

Suponiendo que la variable `Xs` sea una lista con *al menos dos* elementos
podemos recuperar los primeros dos elementos de la siguiente forma:
```erlang
 [A, B | Resto] = Xs,
```
Donde `A` tendrá el primer elemento de `Xs`, `B` el segundo y en `Resto` estarán los valores restantes de `Xs`.

Recuerden que el pattern matching puede fallar, y obtendrán un error de ejecución del programa.

En un programa un poco más grande y un ejercicio es:
```erlang
-module(listas).
-export([play/0]).

primerosDos(Xs) ->
    [A, B | Resto ] = Xs,
    io:format("Me llego una lista cuyos primeros dos elementos son~n"
             "\t + ~p ~n \t + ~p~n",[A, B]).

ultimo(Xs) ->
    io:format("Ejercicio~n").

play() ->
    Xs = [1 , 2 ,3 , banana, manzana, pera],
    primerosDos(Xs),
    ultimo(Xs). % deberia ser `pera`
```

En general la información útil de la lista es saber si es vacía `[]` o si tiene
al menos un elemento `[Hd | Tl]`. Esto se debe que la idea de las listas es
justamente acceder a los datos de forma secuencial, y por ende van a ir sacando
elementos de esa manera.


# Pattern Matching en Átomos

Finalmente en Erlang usaremos mucho pattern matching en átomos, principalmente
los usaremos para enviarnos mensajes entre procesos. Pero pueden ser utilizados para encapsular en una función varias definiciones útiles.

Por ejemplo se puede definir la función `area/2`:
```erlang
area(cuadrado, {Lado}) ->
    Lado * Lado;
area(triangulo,{Base, Altura}) ->
    Base * Altura / 2;
area(OtrasFiguras, Info) -> io:format("Completar").
```
