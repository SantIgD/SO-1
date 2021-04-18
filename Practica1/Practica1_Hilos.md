# Introducción a POSIX Threads

*POSIX Threads* (o simplemente PThreads) es un estándar POSIX para la creación,
manejo y destrucción de hilos. Todas las funciones se encuentran en la librería
con *header* `pthread.h`.

Para compilar un programa C usando PThreads se le debe indicar al compilador gcc
que *linkee* la librearía PThreads con la opción `-lpthread`.

Por ejemplo, para compilar un archivo llamado `ejemplo.c` lo haríamos de la siguiente forma:
```Bash
$ gcc -o ejemplo ejemplo.c -lpthread
```

## Manejo de Threads

La librería *PThread* define un nuevo tipo de datos en C llamado `pthread_t` para
representar a los threads. En ésta sección repasaremos las funciones que vamos a
utilizar para manipular los hilos.

### Creation de Threads

Para la creación de PThreads utilizaremos la función
`pthread_create` con la siguiente signatura:
```C
int pthread_create( pthread_t *thread
                  , const pthread_attr_t *attr
                  , void *(*start_routine) (void *)
                  , void *arg);
```

Que toma como argumentos:

+ un elemento `pthread_t *thread` que apunta al nuevo hilo creado,
+ un puntero a una estructura de atributos `const pthread_attr_t * attr`.
Pequeña estructura con parámetros utilizados al momento de crear el thread. En
el caso que sea `NULL` se utilizan valores por defecto,
+ la rutina a ejecutar,
+ posibles argumentos que necesite la rutina para la ejecución.

En el caso que el llamado a `pthread_create` sea *exitoso*, éste retorna `0`.
Mientras que en el caso de **fallar** se retorna un *número de error*, dejando
además el argumento `thread` con un valor indefinido.
Los errores pueden encontrarlos en el manual de `pthread_create`.

### Terminación de Hilos

Para la terminación de Hilos se utiliza la función **bloqueante** `pthread_join`
que espera a que un hilo dado termine, y sí ya terminó continua inmediatamente.
```C
int pthread_join(pthread_t thread
                , void **retval);
```

Que toma dos argumentos: el hilo a esperar, `pthread_t thread`, y un
puntero a un puntero a `void`, `void **retval`, que se
utiliza para alojar el resultado de la ejecución del
hilo `thread`.

En el caso que el llamado sea exitoso, éste retorna `0`, mientras que en
el caso de fallar retorna un número de error. Los números de error los pueden
encontrar en el manual de `pthread_join`.

Para utilizar correcta el valor de retorno, el hilo que finaliza su ejecución
**no debe utilizar `return`** sino que debe utilizar la función `pthread_exit`.

### Pequeño Ejemplo de Creación de Hilos
Como ejemplo se muestra la creación de dos hilos, que simplemente muestran un
número en pantalla, y se espera a que terminen. ¿Qué pasa cuando se lo ejecuta
repetidas veces?.

```C
#include<pthread.h>
#include<stdio.h>
#include<assert.h>

void* printEcho(void *i){
  /* Interpretamos la entrada como un entero */
  int arg = *((int*)i);
  /* Mostramos un mensaje */
  printf("ECHO:%d!\n",arg);

  return NULL;
}

int main(int argc,char **argv){
  pthread_t thread_id[2];
  int arg1=1, arg2=2;
  void *res;

  printf("Creamos dos hilos!\n");

  /* Creamos dos hilos */
  assert(! pthread_create(&thread_id[0], NULL, printEcho, (void*)&arg1));
  assert(! pthread_create(&thread_id[1], NULL, printEcho, (void*)&arg2));

  printf("Esperamos a que terminen...\n");

  /* Esperamos a que terminen */
  assert(! pthread_join(thread_id[0], &res));
  assert(! pthread_join(thread_id[1], &res));

  printf("Terminamos!\n");

  return 0;
}
```

Lo compilamos *activando el link de pthreads* y lo ejecutamos.
```Bash
$ gcc -o prtinEcho printEcho.c -lpthread
$ ./printEcho
```

Para ejecutar múltiples veces la ejecución del ejemplo puede utilizar
el siguiente *script* en Bash.
```Bash
$ while true; do ./printEcho; done
```


## PThread Locks

La librería de *PThread* define unos propios *mutex locks* que evitan *busy waiting*.

### Creación de Mutex Lock

La creación de un mutex lock se puede realizar de dos maneras.

La creación por defecto es mediante el uso de una *macro*.

```C
#include <pthread.h>

...
  pthread_mutex_t mutexLock = PTHREAD_MUTEX_INITIALIZER;
...

```

Hay una forma de no utilizar la configuración por defecto y afinarla a los gustos del usuario.
Para eso recomiendo ver el manual de entrada de `pthread_mutex_init`/`pthread_mutex_destroy`.

### Tomar un Mutex Lock

Para tomar un mutex lock simplemente se invoca a la siguiente función:

```C
#include <pthread.h>

int pthread_mutex_lock(pthread_mutex_t *mutex);
```

### Liberar un Mutex Lock

Para liberar un mutex lock simplemente se invoca a la siguiente función:

```C
#include <pthread.h>

int pthread_mutex_unlock(pthread_mutex_t *mutex);
```

### Ejercicios

+ [Jardín Ornamental](./Ejercicios/JOrnamental.md)
+ [Algoritmo de Filtro](./Ejercicio/Filtro.md) 
+ [Filósofos Comensales](./Ejercicios/DPhilo.md)

## Semáforos de Dijkstra

En la práctica no sólo nos vamos a encontrar con la necesidad de restringir a
secciones de exclusión mutua, es decir usando variables `pthread_mutex_t`. Sino
que también podríamos pensar en que ciertos problemas aceptan a lo sumo cierta
cantidad de hilos.

Por ejemplo, revistando el problema de los filósofos comensales, una posible
solución es impedir que uno de los filósofos intente comer. Es decir,
restringiendo a que $N-1$ filósofos intenten agarrar los cubiertos necesarios.

Los semáforos son otra solución propuesta inicialmente por Dijkstra. Un semáforo
al igual que los semáforos en la vida real servirán para indicar cuando pueden o
no ingresar hilos a una región en particular.

Los semáforos contienen *internamente* un contador, el cual se le asigna un
valor al ser *creado*, y luego *sólo* se puede incrementar o decrementar.
Son básicamente números enteros, con las siguientes funcionalidades:

+ Al momento de crear un semáforo se le asigna un *entero*. Luego de ser creado
  sólo se puede aumentar o decrementar su valor.
+ Cuando un hilo decrementa el valor del semáforo, si éste es negativo, el hilo
  se bloquea a si mismo y no puede continuar hasta que *otro hilo* incremente el
  semáforo.
+ Cuando un hilo incrementa el semáforo, si hay un hilo esperando éste se
  activa. Si hay varios esperando *alguno se activa*.

Cuando un *hilo se bloquea a si mismo* significa que avisa al *scheduler* que
éste ya no puede continuar. El *scheduler* entonces lo detiene de continuar su
ejecución hasta que vuelva a estar activo.

A tener en cuenta:

+ Cuando un hilo aumenta el semáforo es posible que se active otro hilo. Ambos
  hilos se ejecutaran concurrentemente, y **no se puede asumir orden alguno en
  su ejecución**.
+ No se debe asumir que algún hilo está a la espera del semáforo.

Por ende, un semáforo posee el siguiente significado:

+ Si es positivo, éste representa el número de hilos que pueden decrementarlo
  sin bloquearse.
+ Si es negativo, éste representa el número de hilos que lo decrementaron y
  están bloqueados.
+ Si es cero, significa que no hay ningún hilo esperando, pero si alguien lo
  decrementa éste se bloqueará.

### POSIX Thread Semaphores

Por surte la librearía de *Posix Threads* ya nos provee con una implementación
*confiable* de semáforos. Es necesario incluir la el *header* de la librería:
```C
#include <semaphore.h>
```
Y de nuevo *linkear* con `-lpthread`.

Pero no son *exactamente* los semáforos presentados por Dijkstra. Y poseen una
interfaz un tanto diferente.

#### Creación de Semáforos

La creación es con la llamada a la función `sem_init`:
```C
int sem_init(sem_t *sem, int pshared, unsigned int value);
```

Inicializa un semáforo en la dirección a la que apunta `sem`, mientras que el
valor `value` indica el valor inicial del semáforo. El valor `pshared` indica si
el semáforo es compartido entre hilos de un proceso o directamente entre
procesos.

En el caso que `pshared` sea `0`, el semáforo es compartido entre hilos de un
mismo proceso. Se requiere entonces que la dirección en memoria de `pshared` sea
visible para todos los hilos. Esto se logra por ejemplo siendo una variable
global o alocada dinámicamente.

El el caso que `pshread` sea distinta a `0`, el semáforo es compartido entre
procesos. Se quiere entonces que la dirección de memoria de `pshared` esté en
una región compartida por estos.

### Post y Wait

Se presentan dos operaciones sobre los semáforos, `post` y `wait`.

Similar a decrementar el contador del semáforo la librería nos presenta `wait`.

```C
int sem_wait(sem_t *sem);
```

Pero tiene un comportamiento ligeramente distinto. El hilo que llame a
`sem_wait` va a intentar decrementar el contador del semáforo. Ahora tenemos dos
casos:

+ El contador es mayor a `0`, se decrementa el valor, y la función retorna
  inmediatamente.
+ El contador **es** `0`, entonces la llamada se bloquea hasta que sea posible
  decrementar el contador. Es decir, se bloquea hasta que alguien lo haya
  incrementado a un valor mayor a `0`.

Mientras que `post` nos permitirá incrementar el valor y despertar posibles
hilos bloqueados, siguiendo el comportamiento antes mencionado.

```C
int sem_post(sem_t *sem);
```

La librería nos ofrece además funciones como `sem_getvalue`, `sem_trywait` y
`sem_timedwait`. Las pueden buscar utilizando los manuales de cada una de las
funciones.

## Ejercicios

+ [Implementación de Semáforos de Dijsktra](./Ejercicios/Semaforos.md)
+ Implementar los Semáforos de Dijsktra utilizando la librería de Semáforos de POSIX.
+ Implementar variables de exclusión mutua con Semáforos. Es decir, usar *un
  semáforo* para implementar la funcionalidad de *un mutex*.
+ Implementar la solución propuesta para el problema de los filósofos comensales.
+ [Implementación de Barreras](./Ejercicios/Barreras.md)
+ [Problema de los Fumadores](./Ejercicios/Smokers.md)
