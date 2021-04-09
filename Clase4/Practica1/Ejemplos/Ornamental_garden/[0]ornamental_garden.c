/* POSIX Threads */
#include<pthread.h>
/* Assert library */
#include<assert.h>
/* I/O*/
#include<stdio.h>
/*malloc*/
#include<stdlib.h>

/* Constantes */
#define NVisitantes 100000

/*
  Problema introductorio a exclusión mutua.
 */

void * turnstile(void *argV){
  for(int j = 0; j < NVisitantes; j++)
    (*(int *)argV) ++;
  return NULL;
}

int main(void){
  pthread_t ts[2];

  /* Variable compartida */
  int *counter = malloc(sizeof(int));
  *counter = 0;
  /********************/

  /********************/
  /* Se crean 2 Hilos */
  assert(! pthread_create( &ts[0], NULL, turnstile,(void*)(counter)));
  assert(! pthread_create( &ts[1], NULL, turnstile,(void*)(counter)));


  /* Se espera a que terminen */
  assert(!pthread_join(ts[0], NULL));
  assert(!pthread_join(ts[1], NULL));

  /* Se muestra el resultado del día */
  printf("NVisitantes en total: %d\n", *counter);
  /*
    Resultado esperado sería NVisitantes * 2. ¿Siempre entrega el mismo resultado?
   */
  free(counter);
  return 0;
}
