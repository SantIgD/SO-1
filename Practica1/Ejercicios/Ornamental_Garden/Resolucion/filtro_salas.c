#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "filtro_salas.h"

#define NOPATOVA -1
#define MOLS 10

struct filtro {
    int * salas;
    int * patovas;
};

/* Crea un filtro para _n_ hilos */
filtro_t* filtro(unsigned int n){

    filtro_t* filtro = malloc(sizeof(filtro_t));

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
    int salaPretendida=filtro->salas[id];
    if (filtro->patovas[salaPretendida] == NOPATOVA){

        if(salaPretendida != 0)
            filtro->patovas[salaPretendida-1] = NOPATOVA;
        filtro->patovas[salaPretendida] = id;
    }
    while(filtro->patovas[salaPretendida]!= NOPATOVA && filtro->patovas[salaPretendida] != id){
    }

}

/* El hilo _id_ libera el lock */
void filtro_unlock(filtro_t* filtro, int id){

    if(filtro->salas[id] == MOLS-1){ // ya ejecute

        filtro->salas[id] = 0;
        filtro->patovas[MOLS-2] = NOPATOVA; // Libero la utima sala
        
    }else{

        int estoyEnSala = filtro->salas[id]; // avance a la que pretendia
        filtro->salas[id] += 1; // pretendo avanzar a la siguiente
            
    }

    
    
    
    
    
}

void filtro_delete(filtro_t* filtro){

    free(filtro->salas);
    free(filtro->patovas);
    free(filtro);
}