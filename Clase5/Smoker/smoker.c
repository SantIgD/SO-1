
/* Se incluyen las librerías necesarias*/
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <pthread.h>
#include <semaphore.h>

/* Estructura para los argumentos */
struct _argument {
  sem_t tabaco;
  sem_t papel;
  sem_t fosforos;
  sem_t otra_vez;
};

typedef struct _argument args_t;

void agente(void * _args)
{
    printf("Agente: Hola!\n");
  args_t *args = (args_t *) _args;
  for (;;) {
    int caso = random() % 3;
    sem_wait(&args->otra_vez);
    switch (caso) {
      case 0:
            printf("Caso 0: Hola!\n"); 
          sem_post(&args->tabaco);
          sem_post(&args->papel);
          printf("break 0\n");
          break;
      case 1:
      printf("Caso 1: Hola!\n");
          sem_post(&args->fosforos);
          sem_post(&args->tabaco);
          printf("break 1\n");
          break;
      case 2:
      printf("Caso 2: Hola!\n");
          sem_post(&args->papel);
          sem_post(&args->fosforos);
          printf("break 2\n");
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
  printf("Fumador 1: Hola!\n");
  for (;;) {
    sem_wait(&args->tabaco);
    printf("fumador 1 tomo tabaco\n");
    sem_wait(&args->papel);
    printf("fumador 1 tomo papel\n");
    fumar(1);
    sem_post(&args->otra_vez);
    printf("ceder paso agente, fumador 1\n");
  }
  /* Dead code*/
  pthread_exit(0);
}

void *fumador2(void *_arg)
{
  args_t *args = (args_t *) _arg;
  printf("Fumador 2: Hola!\n");
  for (;;) {
    sem_wait(&args->fosforos);
    printf("fumador 2 tomo fosforos\n");
    sem_wait(&args->tabaco);
    printf("fumador 2 tomo tabaco\n");
    fumar(2);
    sem_post(&args->otra_vez);
    printf("ceder paso agente, fumador 2\n");
  }
  /* Dead code*/
  pthread_exit(0);
}

void *fumador3(void *_arg)
{
  args_t *args = (args_t *) _arg;
  printf("Fumador 3: Hola!\n");
  for (;;) {
    sem_wait(&args->papel);
    printf("fumador 3 tomo papel\n");
    sem_wait(&args->fosforos);
    printf("fumador 3 tomo fosforos\n");
    fumar(3);
    sem_post(&args->otra_vez);
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
  sem_init(&args->tabaco, 0, 0);
  sem_init(&args->papel, 0, 0);
  sem_init(&args->fosforos, 0, 0);
  sem_init(&args->otra_vez, 0, 2);
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



/*
fumador1
se queda trabado en tabaco
-
fumador2
se queda trabado en fosforos
-
Agente 
sem_post(&args->fosforos);
-
fumandor3
se queda boqueado en papel
-
fumador2
cogemos fosforos
nos quedamos trabados en tabaco
-
agente
sem_post(&args->tabaco);
-
fumador1
toma tabaco
se queda trabado en papel

nos quedamos sin fumadores y sin agente hyper f


-------

2 veces, sacas 2 tabaco 2 papel, veces 0

2 t 2p, f1 t, f3 p, 1 t 1 p, f2 espera fosforo, f1 espera papel, f3 espera fosforo

f1 fuma, 1t, f2 espera fosforo, f3 espera fosforo

f1 toma tabaco, no tengo material, f1 espera papel, f2 epera fosforo, f3 espera fosforo

1 f 

f1 tabaco, f3 papel, f1 papel, f1 fuma -> veces 1

1 tabaco -> lo agarra f1

tabaco papel


1 tabaco 1 papel, f1 espera papel, f2 espera fosforo y f3 espera fosforo






A -> TP,TP -> 2T,2P -> F1,t F2,p -> 1T,1p -> F1,P, fuma ->  1T -> 


4 materiales, 2 para fumar y 1 agarra algo -> 1 material (veces 1)

F2 trabado en fosforo

tabaco papel, 

F1 tabaco, F3 Pael deadlock





*/