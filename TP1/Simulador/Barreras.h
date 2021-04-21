#ifndef __BARRERAS_H__
#define __BARRERAS_H__

/* Definición de la estructura y sinónimo de tipo.*/
struct cond_barrier;

typedef struct cond_barrier barrier_t;
/************/

barrier_t* barrier_create();
/************/
/* Operaciones*/

/* Creación de una barrera de condición, tomando como argumento la cantidad de
hilos que se van a esperar*/
int barrier_init(barrier_t *barr, unsigned int count);

/* Función *bloqueante* para esperar a los demás hilos */
int barrier_wait(barrier_t *barr);

/* Eliminación de la barrera */
int barrier_destroy(barrier_t *barr);


#endif