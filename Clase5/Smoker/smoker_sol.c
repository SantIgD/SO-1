
/* Se incluyen las librerías necesarias*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

/* Estructura para los argumentos */
struct _argument {
  int tabaco;
  int papel;
  int fosforos;
  pthread_mutex_t candado;
  pthread_cond_t condicion;
  sem_t semaforo;
};

typedef struct _argument args_t;

void agente(void * _args)
{
   
  args_t *args = (args_t *) _args;
  for (;;) {
    printf("Agente: Hola!\n");
    int caso = random() % 3;
    sem_wait(&args->semaforo);
    switch (caso) {
      case 0:
           printf("Caso 0: Hola!\n"); 
          pthread_mutex_lock(&args->candado);
          args->tabaco=1;
          args->papel=1;
          printf("break 0\n");
          pthread_cond_broadcast(&args->condicion);
          pthread_mutex_unlock(&args->candado);
         
          break;
      case 1:
      printf("Caso 1: Hola!\n");
        pthread_mutex_lock(&args->candado);
         args->fosforos=1;
         args->tabaco=1;
          printf("break 1\n");
          pthread_cond_broadcast(&args->condicion);
        pthread_mutex_unlock(&args->candado);
  
          break;
      case 2:
      printf("Caso 2: Hola!\n");
          pthread_mutex_lock(&args->candado);
          args->papel=1;
          args->fosforos=1;
          printf("break 2\n");
          pthread_cond_broadcast(&args->condicion);
          pthread_mutex_unlock(&args->candado);
          
          break;
    }
  }
  /* Dead code */
  return;
}

void fumar(int fumador)
{
    printf("Fumador %d: Puf! Puf! Puf!\n", fumador);
    sleep(1);
}

void *fumador1(void *_arg)
{
  args_t *args = (args_t *) _arg;
 
  for (;;) {
    printf("Fumador 1: Hola!\n");
    pthread_mutex_lock(&args->candado);
    
    while(!(args->tabaco && args->papel)){
        pthread_cond_wait(&args->condicion,&args->candado);
        
    }
    args->tabaco=0;
    printf("fumador 1 tomo tabaco\n");
    args->papel=0;
    printf("fumador 1 tomo papel\n");
    fumar(1);
    pthread_mutex_unlock(&args->candado);
    sem_post(&args->semaforo);
    printf("ceder paso agente, fumador 1\n");
  }
  /* Dead code*/
  pthread_exit(0);
}

void *fumador2(void *_arg)
{
  args_t *args = (args_t *) _arg;
  
  for (;;) {
    printf("Fumador 2: Hola!\n");
    pthread_mutex_lock(&args->candado);
    while(!(args->tabaco && args->fosforos)){
        pthread_cond_wait(&args->condicion,&args->candado);
       
    }
    args->fosforos=0;
    printf("fumador 2 tomo fosforos\n");
    args->tabaco=0;
    printf("fumador 2 tomo tabaco\n");
    fumar(2);
    pthread_mutex_unlock(&args->candado);
    
    sem_post(&args->semaforo);
    printf("ceder paso agente, fumador 2\n");
  }
  /* Dead code*/
  pthread_exit(0);
}

void *fumador3(void *_arg)
{
  args_t *args = (args_t *) _arg;
  
  for (;;) {
    printf("Fumador 3: Hola!\n");
    pthread_mutex_lock(&args->candado);
    while(!(args->papel && args->fosforos)){
        pthread_cond_wait(&args->condicion,&args->candado);
       
    }
    printf("fumador 3 tomo papel\n");
    args->papel=0;
    printf("fumador 3 tomo fosforos\n");
    args->fosforos=0;
    pthread_mutex_unlock(&args->candado);
    fumar(3);
    
    sem_post(&args->semaforo);
    printf("ceder paso agente, fumador 3\n");
  }
  /* Dead code*/
  pthread_exit(0);
}

int main()
{
  /* Memoria para los hilos */
  pthread_t s1, s2, s3;
  /* Memoria dinámica para los argumentos */
  args_t *args = malloc(sizeof(args_t));

  /* Se inicializan los semáforos */
    args->fosforos=0;
    args->papel=0;
    args->tabaco=0;
    pthread_mutex_init(&args->candado,NULL);
    pthread_cond_init(&args->condicion,NULL);
    sem_init(&args->semaforo, 0, 1);
  /************/

  /* Se inicializan los hilos*/
  pthread_create(&s1, NULL, fumador1, (void*)args);
  pthread_create(&s2, NULL, fumador2, (void*)args);
  pthread_create(&s3, NULL, fumador3, (void*)args);
  /************/

  /* Y el agente que provee con los elemetos*/
  agente((void *)args);
  /************/

  return 0;
}