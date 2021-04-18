#ifndef __SEMDIJKSTRA_H__
#define __SEMDIJKSTRA_H__

struct semaphore_t;

typedef struct semaphore_t sem_t;

sem_t* sem_create();

/* Función de creación de Semáforo */
int sem_init(sem_t *sem, int init);

/* Incremento del semáforo. */
int sem_incr(sem_t *sem);

/* Decremento del semáforo. Notar que es una función que puede llegar a bloquear
   el proceso.*/
int sem_decr(sem_t *sem);

/* Destrucción de un semáforo */
int sem_destroy(sem_t *sem);

int get_value(sem_t* sem);

#endif
