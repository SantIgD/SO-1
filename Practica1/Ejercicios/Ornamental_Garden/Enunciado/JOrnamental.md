# El Jardín ornamental

En un jardín ornamental se organizan visitas guiadas y se desea contar cuánta 
gente entra por día. Hay dos molinetes, uno en cada una de las dos entradas y se
ha implementado el sistema para contar los visitantes utilizando \emph{PThreads}.

Se les provee un esqueleto de código que acompaña a éste ejercicio en la carpeta
de [Ejemplos](./Ejemplos/ornamental_garden.c).

Ejercicios:

+ Por cada molinete entran `NVisitantes` personas, pero al
ejecutar el programa verán que el resultado no es `2*NVisitantes`.
¿Por qué? Porque el incremento de la variable contador no es una operación atómica, lo cual produce que no se cumple la propiedad de mutex.

+ Ejecute el programa 5 veces con `NVisitantes` igual a 10. ¿El programa dio el
  resultado correcto siempre? Si esto es así, ¿por qué? Sí, dió correcto, no tengo ni idea por qué.

+ ¿Cuál es el mínimo valor que podría imprimir el programa? Simular dicha
  situación. El mínimo valor que podría imprimir es `NVisitantes`. El caso se daría cuando ambos hilos ingresan a la sección crítica al mismo tiempo en todas las iteraciones, es decir ambos leen el mismo valor, incrementan en 1 y guardan el mismo valor incrementado en 1.


+ Implementar la solución vista en la clase de teoría (Algoritmo de Peterson). [Listo]
+ Implemente una solución utilizando un *pthread_mutex*. [Listo]
