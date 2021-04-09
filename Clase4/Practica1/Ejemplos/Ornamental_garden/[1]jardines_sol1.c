#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>

#define MOLS 2
#define VISITANTES 1000000

/* Cantidad de visitantes en el Jardín */
long int visitantes;

/* Víctima: hilo que no puede entrar en la región crítica */
long int victima;

void *molinete(void *arg){

  long int id = (long int) arg;

  printf("Molinete %ld activo\n", id);

  for(int i = 0; i < VISITANTES; i++){
    { /* tomar_mutex */
      victima = id;
      while(victima == id)
        printf("Esperando %ld",id);
    }
	  //BEGIN: región crítica
    visitantes ++;
	  //END: región crítica
    { /* soltar_mutex */
      /* NOP */
    }
  }

  printf("Molinete %ld inactivo\n", id);
  pthread_exit(EXIT_SUCCESS);
}

int main(){
  pthread_t mols[2];

  /* Abrimos el Jardín */
  visitantes = 0;

  /* Habilitamos los molinetes */
  assert(!pthread_create( &mols[0]
                          , NULL
                          , molinete
                          , (void*) 0));

  assert(!pthread_create( &mols[1]
                          , NULL
                          , molinete
                          , (void*) 1));

  /* Pasan personas */

  /* Se cierra el jardín */
  assert(! pthread_join(mols[0], NULL));
  assert(! pthread_join(mols[1], NULL));

  printf("Visitantes que pasaron en el día %ld =?= %d\n", visitantes, MOLS * VISITANTES);

  return EXIT_SUCCESS;
}
