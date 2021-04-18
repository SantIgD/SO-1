
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "filtro.h"

#define NOPATOVA -1

struct filtro {
    pthread_mutex_t candado;
    int * salas;
    int * patovas;
};

/* Crea un filtro para _n_ hilos */
filtro_t* filtro(unsigned int n){

    filtro_t* filtro = malloc(sizeof(filtro_t));

    pthread_mutex_init(&filtro->candado,NULL);
    //filtro->candado=PTHREAD_MUTEX_INITIALIZER;
    filtro->salas=malloc(sizeof(int) * n);
    filtro->patovas=malloc(sizeof(int) * (n-1));

    for (int i=0; i < n; i++){

        filtro->salas[i] = 0;

        if(i < n-1)
          filtro->patovas[i] = NOPATOVA;
        
    }
    return filtro;
}


/* El hilo _id_ está intentando tomar el lock */
void filtro_lock(filtro_t* filtro, int id){

    pthread_mutex_lock(&filtro->candado);
}

/* El hilo _id_ libera el lock */
void filtro_unlock(filtro_t* filtro, int id){

    pthread_mutex_unlock(&filtro->candado);
}

void filtro_delete(filtro_t* filtro){

    free(filtro->salas);
    free(filtro->patovas);
    free(filtro);
}