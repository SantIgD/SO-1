#include <pthread.h>
#include <stdio.h>
#include<stdlib.h> /STD error output/
#include<unistd.h>
#include<time.h>
#include <math.h>
#define NPUNTOS 30100
#define RADIO 10

double generarRandom(int max){
    double random;

    random = drand48()*max;
    
    return random;
}

void generarCoordenadas(double max,double* coordenadas){
   
   for (int i=0; i<2; i++){
       coordenadas[i] = generarRandom(max);
   }

}


double distancia(double* punto, double* centro){
    double distancia[2];

    for(int i=0; i<2; i++){

        distancia[i] = pow(centro[i] - punto[i],2);
    }
    return sqrt(distancia[0]+distancia[1]);

}


void * generarPunto (void * arg){
    /* La memoria que nos mandan es de un entero */
    
    
    double lado = 2*RADIO;
    double centro[2],punto[2];
    int seed = (int) arg;
    srand48(seed);
    int res=0;

    //printf("[Hilo] %d [Seed] %d \n",(int) arg,seed);

    centro[0] = lado/2;
    centro[1] = lado/2;
    //printf("[centro circunferencia]%f---%f",centro[1],centro[2]);

    generarCoordenadas(lado,punto);

//    printf("[Hilo] %d [Seed] %d [p] (%f,%f) \n",(int) arg,seed,punto[0],punto[1]);

    double distCentro = distancia(punto,centro);
   // printf("[Hilo] %d [Seed] %d [p] (%f,%f) [dist] %f \n",(int) arg,seed,punto[0],punto[1],distCentro);


    if(distCentro <= RADIO){
        (int) arg=-1;
    }
    pthread_exit(EXIT_SUCCESS);
}

int main(int argc,char** argv){

    int puntosCirc=0;
    int inside;
    int args[NPUNTOS];

    // Nombre de los Threads
    pthread_t ths[NPUNTOS];

    /* Argumentos de los hilos */
    //NULL

     /* Crear NTHS hilos */
    
    for (int i=0; i < NPUNTOS; i++){
        args[i]= i;
        if (pthread_create( &ths[i],
            NULL,
            generarPunto,
            (void*) &args[i])
            != 0){
                perror("Fallo la creacion de un Hilo");
                _exit(EXIT_FAILURE);
            }
    }

   

    /* Esperamos a que todos los threads terminen */

    for (int i =0; i<NPUNTOS; i++){

        if (pthread_join(ths[i],NULL) // es bloqueante
            != 0){

                perror("Fallo la espera de un Hilo");
                _exit(EXIT_FAILURE);
        }
        if(args[i]==-1)
            puntosCirc++; 
    }
    puntosCirc=puntosCirc*4;
    double result=((double)puntosCirc/(double)NPUNTOS);
    printf("[pc] %d, Pi = %f\n",puntosCirc/4, result);

    return 0;
}