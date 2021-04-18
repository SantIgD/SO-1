# Cena de los Filósofos (Dijkstra)
Cinco filósofos se sientan alrededor de una mesa redonda y pasan su vida
comiendo y pensando. Cada filósofo tiene un plato de fideos y un tenedor a la
izquierda de su plato. Para comer los fideos son necesarios dos tenedores y cada
filósofo sólo puede tomar los que están a su izquierda y derecha. Primero toman
el que está a su derecha y luego el que está a su izquierda. Si cualquier
filósofo toma un tenedor y el otro está ocupado, se quedará esperando, con el
tenedor en la mano, hasta que pueda tomar el otro tenedor, para luego empezar a
comer.

Una vez que termina de comer deja los tenedores sobre la mesa y piensa por un
momento hasta que luego empieza a comer nuevamente.

![Filósofos en la mesa](./philosophers.eps)

Se les provee un esqueleto de código que acompaña a éste ejercicio en la carpeta
de [Ejemplos](../Ejemplos/philosophers.c).

+ Este programa puede terminar en *deadlock*. ¿En qué situación se
puede dar?

+ Cansados de no comer los filósofos deciden pensar una solución a su problema.
Uno razona que esto no sucederá si alguno de ellos fuese zurdo y tome primero el
tenedor de su izquierda. Implemente esta solución y explique por qué funciona.
