#include <pthread.h>
#include <stdio.h>
#include <stdlib.h> /*STD error output*/
#include <unistd.h>
#include "Barreras.h"

barrier_t* barrera;


void * probandoBarrera (void * arg){
    /* La memoria que nos mandan es de un entero */
    
    
 
    int seed = *(int*) arg;
    printf("%d\n",seed);

    barrier_wait(barrera);

    printf("pase" );
    printf(" %d\n",seed);
  
}

int main(int argc,char** argv){

    int args[5];


    // Nombre de los Threads
    pthread_t ths[5];

    barrera = barrier_create();

    barrier_init(barrera,5);
    /* Argumentos de los hilos */
    //NULL

     /* Crear NTHS hilos */
    
    for (int i=0; i < 5; i++){
        args[i]= i;
        if (pthread_create( &ths[i],
            NULL,
            probandoBarrera,
            (void*) &args[i])
            != 0){
                perror("Fallo la creacion de un Hilo");
                _exit(EXIT_FAILURE);
            }
    }

   

    /* Esperamos a que todos los threads terminen */

    for (int i =0; i<5; i++){

        if (pthread_join(ths[i],NULL) // es bloqueante
            != 0){

                perror("Fallo la espera de un Hilo");
                _exit(EXIT_FAILURE);
            }
        
    }
    
    printf("Muy rico todo\n");
    

    barrier_destroy(barrera);

    return 0;
}