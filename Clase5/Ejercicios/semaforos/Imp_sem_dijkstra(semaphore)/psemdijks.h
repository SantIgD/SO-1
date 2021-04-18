#ifndef __PSEMDIJKS_H__
#define __PSEMDIJKS_H__

struct semaphore_t;

typedef struct semaphore_t p_sem_t;

p_sem_t* p_sem_create();

/* Función de creación de Semáforo */
int p_sem_init(p_sem_t *sem, int init);

/* Incremento del semáforo. */
int p_sem_incr(p_sem_t *sem);

/* Decremento del semáforo. Notar que es una función que puede llegar a bloquear
   el proceso.*/
int p_sem_decr(p_sem_t *sem);

/* Destrucción de un semáforo */
int p_sem_destroy(p_sem_t *sem);

int p_get_value(p_sem_t* sem);

#endif
