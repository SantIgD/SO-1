# El Jardín ornamental

En un jardín ornamental se organizan visitas guiadas y se desea contar cuánta 
gente entra por día. Hay dos molinetes, uno en cada una de las dos entradas y se
ha implementado el sistema para contar los visitantes utilizando \emph{PThreads}.

Se les provee un esqueleto de código que acompaña a éste ejercicio en la carpeta
de [Ejemplos](./Ejemplos/ornamental_garden.c).

Ejercicios:

+ Por cada molinete entran `NVisitantes` personas, pero al
ejecutar el programa verán que el resultado no es `2*NVisitantes`.
¿Por qué?.
+ Ejecute el programa 5 veces con `NVisitantes` igual a 10. ¿El programa dio el
  resultado correcto siempre? Si esto es así, ¿por qué?
+ ¿Cuál es el mínimo valor que podría imprimir el programa? Simular dicha
  situación.
+ Implementar la solución vista en la clase de teoría (Algoritmo de Peterson). 
+ Implemente una solución utilizando un *pthread_mutex*.
