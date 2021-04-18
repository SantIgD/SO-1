#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>

#define MOLS 2
#define VISITANTES 1000000

long int visitantes;

void *molinete(void *arg){
  pthread_mutex_t *candado = (pthread_mutex_t *) arg;

  for(int i = 0; i < VISITANTES; i++){
    pthread_mutex_lock(candado);
    visitantes ++;
    pthread_mutex_unlock(candado);
  }

  pthread_exit(EXIT_SUCCESS);
}

int main(){
  pthread_t mols[2];
  pthread_mutex_t candado = PTHREAD_MUTEX_INITIALIZER;

  visitantes = 0;

  /* Habilitamos los molinetes */
  assert(!pthread_create( &mols[0]
                          , NULL
                          , molinete
                          , (void*) &candado));

  assert(!pthread_create( &mols[1]
                          , NULL
                          , molinete
                          , (void*) &candado));

  /* Pasan personas */

  /* Se cierra el jardín */
  assert(! pthread_join(mols[0], NULL));
  assert(! pthread_join(mols[1], NULL));

  printf("Visitantes que pasaron en el día %ld =?= %d\n", visitantes, MOLS * VISITANTES);

  return EXIT_SUCCESS;
}
