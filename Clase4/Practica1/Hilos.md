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

## Ejercicios

+ [Jardín Ornamental](./Ejercicios/JOrnamental.md)
+ [Algoritmo de Filtro](./Ejercicio/Filtro.md) 
+ [Filósofos Comensales](./Ejercicios/DPhilo.md)
