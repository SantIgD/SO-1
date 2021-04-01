#include <pthread.h>
#include <stdio.h>
#include<stdlib.h> /*STD error output*/
#include<unistd.h>
#include<time.h>
#include <math.h>
#define NPUNTOS 10
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



double modulo(double coordenadas[2]){
    double modulo=0;

    for(int i=0; i<2; i++){
        modulo += pow(coordenadas[i],2);
    }

    return sqrt(modulo);

}

double distancia(double* punto, double* centro){
    double distancia[2];

    for(int i=0; i<2; i++){

        distancia[i] = centro[i] - punto[i];
    }

    return modulo(distancia);

}


void * generarPunto (void * arg){
    /* La memoria que nos mandan es de un entero */
    
    
    int lado = 4*RADIO;
    double* centro = malloc(sizeof(double) * 2);
    double* punto=malloc(sizeof(double) * 2);
    int seed = *(int*) arg;
    srand48(seed);
    int res=0;

    //printf("[Hilo] %d [Seed] %f \n",*(int*) arg,seed);

    centro[1] = lado/2;
    centro[2] = lado/2;

    generarCoordenadas(lado,punto);

    printf("[Hilo] %d [Seed] %d [p] (%f,%f) \n",*(int*) arg,seed,punto[0],punto[1]);

    double distCentro = distancia(punto,centro);

    //printf("mod [(%f,%f)-(%f,%f)] = %f \n",lado/2,lado/2,punto[1],punto[2],distCentro);

    if(distCentro <= RADIO){
        res+=1;
    }

    /*if(res == 1){
        printf("(%f,%f) pertenece al circulo\n",punto[1],punto[2]);
    }else{
        printf("(%f,%f) no pertenece al circulo\n",punto[1],punto[2]);
    }
    */

    free(punto);
    free(centro);

    pthread_exit(res);
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

        if (pthread_join(ths[i],(void*)&inside) // es bloqueante
            != 0){

                perror("Fallo la espera de un Hilo");
                _exit(EXIT_FAILURE);
        }else{
            //printf("[in] %d\n",inside);
            if (inside == 1){
                //printf("[pc] %d\n",puntosCirc);
                //getchar();
                puntosCirc =10;
                //printf("[pc] %d\n",puntosCirc);
            }
            //printf("[pc] %d\n",puntosCirc);
        }
    }


    printf("[pc] %d, Pi = %d\n",puntosCirc, (puntosCirc)*4/NPUNTOS);

    return 0;
}
