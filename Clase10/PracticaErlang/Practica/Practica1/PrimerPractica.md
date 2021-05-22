# Introducción a Erlang

[Erlang](https://www.erlang.org/) es un lenguaje funcional de propósito general
con soporte nativo para la concurrencia, el desarrollo de software distribuido y
tolerante a fallas.

+ Es funcional aunque de tipado dinámico, se basa en la declaración de
  funciones utilizando pattern matching y permite funciones recursivas.
+ Diseñado para el desarrollo industrial de sistemas con requerimientos de
  tiempo real
+ Manejo explícito de la concurrencia y Mensajes asíncronos
+ Se basa en el Modelo de Actores
+ Se ejecuta sobre una máquina virtual *BEAM*, esto permite su portabilidad.

Los conceptos básicos los trabajaremos en lo que queda de la materia.

## Mención al Modelo de Actores

Se basa en el modelo de actores, donde la entidad fundamental de computo se
realiza a través de actores, y la comunicación entre ellos.

Al recibir un mensaje un actor puede:

+ Crear otros actores
+ Tomar decisiones locales
+ Enviar mensajes a otros actores
+ Decidir como responder al mensaje recibido

**Toda la comunicación entre actores es por envío y recepción de mensajes**.
Esto hace que toda comunicación sea:

+ Explícita
+ Traceable
+ Segura

De esta manera los actores definen cómo se computa en el sistema, y estos a su
vez puede ser modificados, intercambiados, y el sistema puede escalar
libremente. Un actor no necesita saber si está ejecutando concurrentemente en
una computadora, o de forma distribuida, sólo sabe cómo responder y actuar al
momento que le llegue un mensaje.

## Primer Programa en Erlang

Para ir introduciendo los conceptos básicos de un programa Erlang, partiremos de
un ejemplo.

Creamos un archivo `factorial.erl`:

```Erlang
-module(factorial).
-export([factorial/1]).
```

Declaramos que el modulo se llama `factorial` y que exporta una lista de
funciones compuesta por una sola llamada `factorial` y que espera un sólo
argumento.
Notar que todas las declaraciones de un termino en Erlang terminan con un punto.

```Erlang
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).
```

Declaramos entonces la función factorial de forma recursiva. La función la
declaramos por pattern matching donde los distintos casos son separados por
punto y coma (`;`). A su vez las variables comienzan con letras mayúsculas.

Este programa lo podemos cargar y ejecutar con la ayuda de la *Shell* de Erlang.
Abrimos una terminal en la misma carpeta que contiene el archivo `factorial.erl`
y ejecutamos `erl`.

```bash
$ erl
Erlang/OTP 22 [erts-10.7] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:1] [hipe]

Eshell V10.7  (abort with ^G)
1> c(factorial).
{ok,factorial}
2> factorial:factorial(5).
120
3> q().
```

Con la función `c()` realizamos la carga del modulo `factorial`.
Recibimos una tupla con un mensaje de que el modulo se cargó con éxito y
podemos llamar entonces a la función factorial dentro del modulo.
Para salir llamamos a la función `q()`.
Notar nuevamente la presencia de los puntos al final de cada sentencia.

Para ver más funciones y operadores de la shell puede llamar a la función
`help()`.

A su vez abrir un menú de control con `^G`, e ingresando `h` para observar los
posibles comandos.

## Tipos de Datos

Erlang provee los siguientes tipos de datos

+ Tipos de Datos Constantes: los tipos básicos e indivisibles 
  - Números: Números literales como `1`, `-123`, `23e10`, etc. 
  Estos pueden ser enteros o flotantes.
  - Átomos: por ejemplo `atomo`, `pepgrillo`, `hola`, etc. Son constantes con
    nombres.
+ Tipos de Datos Compuestos: tipos de datos que agrupan otros tipos de datos.
  - Tuplas : `{1,a,2.3}`, `{}`, `{1,2,3,4}`, `{aa,bb,w}`.
  Las tuplas son agrupan un número concreto de datos heterogéneos.
  - Listas: Las listas son utilizadas para almacenar un numero variable de
    elementos, y gracias al tipado dinámico en Erlang las listas son
    heterogéneas. Por ejemplo: `[]`,`[1,2,3,a,b,c]`, `[a,1,{1,2,3},'hello']`.

Las listas y las tuplas pueden ser anidadas generando datos más complejos.

Los valores en Erlang pueden ser guardados en *variables*, y para diferenciarlas
de los átomos siempre comienzan con una letra en mayúsculas. Por ejemplo

```Erlang
	X = {libro, prefacio, reconocimientos, contenido,
		{capitulos, [ {capitulo, 1, 'Tutorial de Erlang'}
                            , {capitulo, 2, ... }
			    ]
                }},
```

La asignación en Erlang, al igual que en otros lenguajes declarativos, se
realiza una sola vez. Por ejemplo, la variable `X` en el fragmento de código
anterior no se le podrá asignar otro valor dentro de la misma declaración.

## Concurrencia

Erlang es un lenguaje de programación concurrente el cual provee un mecanismo
de concurrencia explícito. Esto además significa que no sólo no es necesario a
recurrir a librerías para el manejo de programas paralelos y concurrentes, sino
que al ser ejecutado sobre una máquina virtual será el ecosistema de Erlang
quien provea el paralelismo y **no el sistema operativo**.

Las primitivas que ofrece son (entre otras):

+ La primitiva `spawn/3` que da comienzo a un nuevo proceso,
+ La primitiva `send/2` que envía un mensaje a un proceso,
+ La primitiva `receive/0` que recibe un mensaje de otro proceso.

--------------------------------------------------------------------------------

# Ejercicios:

## Pattern Matching

Podes revisar el tutorial introductorio a [Pattern Matching](./PatternMatching.md) para refrescar el concepto.

+ Escribir un modulo llamado `temp` donde se definan:
  - Una función `temp:f2c(F)` que tome la temperatura en [Grados
    Fahrenheit](https://es.wikipedia.org/wiki/Grado_Fahrenheit) y devuelve su
    equivalente en [Grados Celsius](https://es.wikipedia.org/wiki/Grado_Celsius)
  - Una función `temp:c2f(C)` que haga el proceso inverso a `temp:f2c`
  - Escribir una única función `temp:convertir/1` que encapsule ambos
    comportamientos, dependiendo del primer argumento.
    Ejemplos de aplicación
    ```erlang
    > temp:convertir({c, 100}).
    => {f, 212}
    > temp:convertir({f, 212}).
    => {c, 100}
    ```
+ Escribir un modulo llamado `mates` donde definan una función
`perimetro/1` que compute el perímetro de diferentes formas.
Las formas que puede tomar como argumento son:
```erlang
{square,Side}
{circle,Radius}
{triangle,A,B,C}
```

## Funciones Recursivas

Escribir un modulo llamad `listas` donde definan:

+ La función `min/1`: que tome una lista y devuelva el minino de la lista.
+ La función `max/1`: que tome una lista y devuelva el máximo de la lista.
+ La función `min_max/1`: que tome una lista y devuelva una dupla con el minimo
  y máximo.
+ Escribir la función `map/2` que toma una función y una lista, y retorne el
  resultado de aplicarle la función a cada uno de los elementos de la lista.
  
Erlang permite funciones anónimas, es decir, funciones que no tienen nombre.
Estas funciones anónimas son muy útiles para utilizarlas en conjunto con `map/2`. Veamos un ejemplo de una función que multiplica por dos.
```erlang
Duplicar = fun(X) -> X * 2 end.
```

En el caso de querer una `map` con funciones que sí tengan nombre, deberán hacerlo también con `fun`. Por ejemplo
```erlang
> listas:map(fun Modulo:Nombre/Aridad, Lista).
```

Notar que como mencioné en la clase el *nombre* de una función tiene tres componentes: el modulo donde está definida, el nombre, y la árida.

+ La función `foreach/2` que toma un procedimiento (una función que no retorna
  ningún valor) y un lista. El resultado de invocar a `foreach(F, Lista)` es el
  de aplicar `F` a cada elemento de la lista `Lista` terminando con un `ok`.
  Un ejemplo de aplicación de `foreach` es el siguiente:
  ```erlang
  > alto:foreach(fun(X)->io:format("~p~n",[X]) end, ["Hola","todo bien?"]).
  "Hola"
  "todo bien?"
  ok
  ```
  
+ Escribir una función `fecha_actual/0` que retorne la fecha actual en formato
  "DDMMAA". Hint: Pueden ser útiles las funciones BIF: `date/0`, `time/0` y
  `integer_to_list/1`.

## Temporización en Erlang

+ Implementar una función `wait/1` que tome como argumento una cantidad
de milisegundos y espere ese tiempo. Hint: timers.

+ Implementar una función `cronometro/3` que tome tres argumentos:
  - Una función `Fun`
  - Una fecha `Hasta`
  - Un periodo `Periodo`
y ejecute la función `Fun/0` cada `Periodo` milisegundos hasta que hayan pasado
`Hasta` milisegundos **sin bloquear el proceso**.

Por ejemplo: `cronometro(fun () -> io:format("Tick~n") end, 60000, 5000).`
Que imprimirá `Tick~n` cada 5 segundos durante un minuto.

## Interacción Entre Procesos

+ Escribir una función `start/2` que tome un entero `M` y un mensaje `Msj`, cree
  dos procesos que envíen `M` mensajes `Msj` entre ellos y terminen, ambos
  correctamente.

+ Escribir una función `startAnillo/3` que tome dos entero `M`,`N` y un mensaje `Msj`, genere `N` procesos en forma de anillo, y haga circular el mensaje `Msj` unas `M` veces. Todos los procesos deberán terminar correctamente.

![Imagen de Anillo](./ex2.gif "Anillo en Erlang")

NB: Cuando digo que todos los procesos deberán terminar correctamente me refiero
a que cada proceso puedo evaluar la función que le corresponde completamente, y
no que se queda esperando un mensaje o en deadlock.

--------------------------------------------------------------------------------

# Aclaraciones hechas en el Canal de Zulip

## Tiempo de Espera para `receive`

La primitiva `receive` en Erlang se puede aumentar *opcionalmente* con una
*clausula de Timeout*. Es decir, que si en el caso que un proceso se ponga a
esperar por un mensaje y no llegue ninguno (ni haya alguno en el buzón), espere
una cierta cantidad de *milisegundos* y ejecute una clausula adicional. De la
siguiente forma:

```erlang
receive
    Mensaje1 ->
        Cuerpo1;
    Mensaje2 ->
        Cuerpo2;
    ....
after
    Tiempo ->
        CuerpoTimeout
end
```

Hint: muy útil para hacer `wait/0`.

## Guardas en el Pattern Matching

Más información útil, el Pattern Matchin de Erlang acepta guardas para
establecer propiedades sobre los argumentos de una función (o de los mensajes a
recibir). La sintaxis sería por ejemplo:

```erlang
nombreFunción(Arg1, Arg2,...) when Guarda1(Arg1, Arg2, ...) -> Cuerpo1;
nombreFunción(Arg1,Arg2,...) when Guarda2(Arg1,Arg2, ...) -> Cuerpo2;
..... 
```

Un ejemplo concreto sería:
```erlang
min(X,Y) when X < Y -> X;
min(_,Y) -> Y.
```

## Ejercicio Adicional Buzón de Mensajes

Cuál es el resultado de la invocación a `probandoBuzon/0`?

```erlang
what(msj1) ->
    io:format("Llegó el mensaje1 !! ~n");
what(msj2) ->
    io:format("Llegó el mensaje2 !! ~n");
what(_) ->
    io:format("Llegó cualquier cosa Martín!~n").

buzon() ->
    receive
        Msj -> what(Msj), buzon()
    after
        1000 -> io:format("Hello darkness my old friend~n")
    end.

pruebaBuzon()->
    PBuzon = spawn(practica, buzon, []),
    wait(100),
    PBuzon ! msj1,
    PBuzon ! cualca,
    PBuzon ! msj2,
    io:format("Fin de Prueba de Buzon~n").
```
