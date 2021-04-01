#include <pthread.h>
#include <stdio.h>
#include<stdlib.h> /*STD error output*/
#include<unistd.h>
#define NTHS 10 


void * sayhi (void * arg){
    /* La memoria que nos mandan es de un entero */
    int memarg= * (int *) arg;

    printf("Hola soy %d\n",memarg);

    pthread_exit(EXIT_SUCCESS);
}

int main(int argc, char** argv){
    // Nombre de los Threads
    pthread_t ths[NTHS];

    /* Argumentos de los hilos */
    int args[NTHS];

    for (int i=0; i < NTHS; i++){

        args[i] = i;
    }

    /* Crear NTHS hilos */
    for (int i=0; i < NTHS; i++){
        if (pthread_create( &ths[i],
            NULL,
            sayhi,
            (void *) (&args[i]))
            != 0){
                perror("Fallo la creacion de un Hilo");
                _exit(EXIT_FAILURE);
            }
    }

    /* Esperamos a que todos los threads terminen */

    for (int i =0; i<NTHS; i++){

        if (pthread_join(ths[i],NULL) // es bloqueante
            != 0){

                perror("Fallo la espera de un Hilo");
                _exit(EXIT_FAILURE);
            }else{
                printf("Hilo [%d] termino\n",i);
            }
    }

    printf("Listo, muy rico todo!\n");

    return 0;


}