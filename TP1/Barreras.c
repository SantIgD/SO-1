#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include "Barreras.h"

/* Definición de la estructura y sinónimo de tipo.*/
struct cond_barrier{

    int cantHilos,contador;
    pthread_cond_t terminoBloque;
    pthread_mutex_t candadoEntreBloques;


};

/************/

/************/
/* Operaciones*/

barrier_t* barrier_create(){

    return malloc(sizeof(barrier_t));
    
}

/* Creación de una barrera de condición, tomando como argumento la cantidad de
hilos que se van a esperar*/
int barrier_init(barrier_t *barr, unsigned int count){

    barr->cantHilos = count;

    barr->contador  = 0;

    pthread_cond_init(&barr->terminoBloque,NULL);
    pthread_mutex_init(&barr->candadoEntreBloques,NULL);
    
    return 0;
}

/* Función *bloqueante* para esperar a los demás hilos */
int barrier_wait(barrier_t *barr){

    pthread_mutex_lock(&barr->candadoEntreBloques);

    if (barr->contador < barr->cantHilos-1 ){

        barr->contador++;
        pthread_cond_wait(&barr->terminoBloque,&barr->candadoEntreBloques);
    }else{
        barr->contador = 0;
        pthread_cond_broadcast(&barr->terminoBloque);
    }

    pthread_mutex_unlock(&barr->candadoEntreBloques);

    return 0;
}

/* Eliminación de la barrera */
int barrier_destroy(barrier_t *barr){

    pthread_cond_destroy(&barr->terminoBloque);
    pthread_mutex_destroy(&barr->candadoEntreBloques);

    free(barr);

    return 0;
    
}

