#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "semdijkstra.h"

#define MOLS 2
#define VISITANTES 1000000

long int visitantes;

sem_t* sem;

void *molinete_entrada(void *arg){

  for(int i = 0; i < VISITANTES; i++){
    
    sem_incr(sem);
    
  }

  pthread_exit(EXIT_SUCCESS);
}


void *molinete_salida(void *arg){
  

  for(int i = 0; i < VISITANTES; i++){
    
    sem_decr(sem);
    

  }

  pthread_exit(EXIT_SUCCESS);
}




int main(){

  pthread_t mols[2];
  
  sem=sem_create();
  sem_init(sem,0);

  //printf("EL valor es %d\n",sem->value);
  //sem->candado = PTHREAD_MUTEX_INITIALIZER;
  //sem->rojo = PTHREAD_COND_INITIALIZER;

  visitantes = 0;

  int i = 1;

  sem_incr(sem);
  sem_decr(sem);

  /* Habilitamos los molinetes */
  assert(!pthread_create( &mols[0]
                          , NULL
                          , molinete_entrada
                          , (void *) &i));

  assert(!pthread_create( &mols[1]
                          , NULL
                          , molinete_salida
                          , (void *) &i));

  /* Pasan personas */

  /* Se cierra el jardín */
  assert(! pthread_join(mols[0], NULL));
  assert(! pthread_join(mols[1], NULL));

  printf(" 0 =?= %d\n",get_value(sem));

  return EXIT_SUCCESS;
}
