#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "filtro_salas.h"

#define NOPATOVA -1
#define MOLS 3

int pasaron  = 0;
int cedieronPaso = 0;
int pasoAlguien=0;

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

    int voyYo = 0;
    int salaPretendida=filtro->salas[id];

    while (salaPretendida != MOLS-1){

        while (voyYo == 0){
            
            pasoAlguien = 0;

            if (cedieronPaso != MOLS-1-pasaron)
                cedieronPaso +=1;
            else{
                voyYo=1;
                cedieronPaso=0;
                pasaron+=1;
            }

        while(!pasoAlguien && voyYo == 0);

        }

        if (filtro->patovas[salaPretendida] == NOPATOVA){

            if(salaPretendida != 0)
                filtro->patovas[salaPretendida-1] = NOPATOVA;

            filtro->patovas[salaPretendida] = id;
        }

        pasoAlguien = 1; // pase solito pa, vengan de a 1

        while(filtro->patovas[salaPretendida] != NOPATOVA && filtro->patovas[salaPretendida] != id);

        filtro->salas[id]+=1;


    }

}

/* El hilo _id_ libera el lock */
void filtro_unlock(filtro_t* filtro, int id){
    
    filtro->salas[id] = 0;
    filtro->patovas[MOLS-2] = NOPATOVA; // Libero la utima sala
}

void filtro_delete(filtro_t* filtro){

    free(filtro->salas);
    free(filtro->patovas);
    free(filtro);
}