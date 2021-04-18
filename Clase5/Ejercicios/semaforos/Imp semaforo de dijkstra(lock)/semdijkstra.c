#include <stdlib.h>
#include <pthread.h>
#include <stdio.h>
#include "semdijkstra.h"

struct semaphore_t{

    int value;
    pthread_mutex_t candado;
    pthread_cond_t rojo;
    
};

sem_t* sem_create(){

    return malloc(sizeof(sem_t));
}

/* Función de creación de Semáforo */
int sem_init(sem_t *sem, int init){

    sem->value   = init;
    printf("EL valor es %d\n",sem->value);
    pthread_cond_init(&sem->rojo,NULL);
    pthread_mutex_init(&sem->candado,NULL);

    return 0;


}

/* Incremento del semáforo. */
int sem_incr(sem_t *sem){

    pthread_mutex_lock(&sem->candado);
    sem->value++;

    pthread_cond_signal(&sem->rojo); // De esta forma luego del unlock todos los hilos van a poder competir en igualdad de condiciones
    pthread_mutex_unlock(&sem->candado);


    return 0;
}

/* Decremento del semáforo. Notar que es una función que puede llegar a bloquear
   el proceso.*/
int sem_decr(sem_t *sem){

    pthread_mutex_lock(&sem->candado);
    
    sem->value--;
    
    if (sem->value < 0){
        pthread_cond_wait(&sem->rojo, &sem->candado);
    }

    pthread_mutex_unlock(&sem->candado);
}

/* Destrucción de un semáforo */
int sem_destroy(sem_t *sem){

    pthread_cond_destroy(&sem->rojo);
    pthread_mutex_destroy(&sem->candado); 
    free(sem);
    return 0;
}

int get_value(sem_t *sem){

    return sem->value;
}
