#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "filtro.h"

#define MOLS 10
#define VISITANTES 1000000


/*creamos el f*/
filtro_t* f ;


/* Cantidad de visitantes en el Jardín */
long int visitantes;

void *molinete(void *arg){

  long int id = (long int) arg;

  printf("Molinete %ld activo\n", id);

  for(int i = 0; i < VISITANTES; i++){
    { /* tomar_mutex */
      filtro_lock(f,id);
    }
	  //BEGIN: región crítica
    visitantes ++;
	  //END: región crítica
    { /* soltar_mutex */
      filtro_unlock(f,id);
    }
  }

  printf("Molinete %ld inactivo\n", id);
  pthread_exit(EXIT_SUCCESS);
}


int main(){
  pthread_t mols[MOLS];

  /* Abrimos el Jardín */
  visitantes = 0;

  /* Habilitamos los molinetes */
  /*
  assert(!pthread_create( &mols[0]
                          , NULL
                          , molinete
                          , (void*) 0));

  assert(!pthread_create( &mols[1]
                          , NULL
                          , molinete
                          , (void*) 1));
*/

  f = filtro(MOLS);
  int i=0;
  for(;i<MOLS;i++)
     assert(!pthread_create( &mols[i]
                          , NULL
                          , molinete
                          , (void*) &i));


  /* Pasan personas */

  /* Se cierra el jardín */
  for(i=0;i<MOLS;i++)
    assert(! pthread_join(mols[i], NULL));
    

  printf("Visitantes que pasaron en el día %ld =?= %d\n", visitantes, MOLS * VISITANTES);

  return EXIT_SUCCESS;
}
