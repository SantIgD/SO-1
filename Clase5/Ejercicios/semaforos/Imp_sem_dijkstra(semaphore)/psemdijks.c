#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>
#include "psemdijks.h"
#include <semaphore.h>

struct semaphore_t{

    int hilosEsperando;
    pthread_mutex_t candado;
    sem_t semaphore;
    
};

p_sem_t* p_sem_create(){

    return malloc(sizeof(p_sem_t));
}

/* Función de creación de Semáforo */
int p_sem_init(p_sem_t *sem, int init){

    sem->hilosEsperando   = 0;
    pthread_mutex_init(&sem->candado,NULL);
    sem_init(&sem->semaphore, 0, init);

    return 0;


}

/* Incremento del semáforo. */
int p_sem_incr(p_sem_t *sem){

    pthread_mutex_lock(&sem->candado);

    if(sem->hilosEsperando < 0)
        sem->hilosEsperando++;


    
    sem_post(&sem->semaphore);

    pthread_mutex_unlock(&sem->candado);
    


    return 0;
}

/* Decremento del semáforo. Notar que es una función que puede llegar a bloquear
   el proceso.*/
int p_sem_decr(p_sem_t *sem){

    int* valor;
    pthread_mutex_lock(&sem->candado);

    sem_getvalue(&sem->semaphore,valor);
    
    
    if (valor == 0){
        sem->hilosEsperando--;
    }

    pthread_mutex_unlock(&sem->candado);
}

/* Destrucción de un semáforo */
int p_sem_destroy(p_sem_t *sem){

    pthread_mutex_destroy(&sem->candado); 
    sem_destroy(&sem->semaphore);
    free(sem);
    return 0;
}

int p_get_value(p_sem_t *sem){

    return sem->hilosEsperando;
}
