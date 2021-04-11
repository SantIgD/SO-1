#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "filtro.h"

#define MOLS 10
#define VISITANTES 1000000
/* Intenciones */
#define DO 1
#define DONT 0

/* Cantidad de visitantes en el Jardín */
long int visitantes;

/* Banderas de Intenciones */
long int intencion[MOLS];

/* Víctima: hilo que no puede entrar en la región crítica */
long int cedoPaso;

filtro_t filtro = filtro();

void *molinete(void *arg){

  long int id = (long int) arg;

  printf("Molinete %ld activo\n", id);

  for(int i = 0; i < VISITANTES; i++){
    { /* tomar_mutex */
      long int otro = 1 - id;
      cedoPaso = id;
      intencion[id] = DO;
      while(intencion[otro] == DO && cedoPaso == id);
    }
	  //BEGIN: región crítica
    visitantes ++;
	  //END: región crítica
    { /* soltar_mutex */
      intencion[id] = DONT;
    }
  }

  printf("Molinete %ld inactivo\n", id);
  pthread_exit(EXIT_SUCCESS);
}

void *molinete2(void *arg){

  long int id = (long int) arg;

  printf("Molinete %ld activo\n", id);

  for(int i = 0; i < VISITANTES; i++){
    { /* tomar_mutex */
      filtro_lock(filtro,id);
    }
	  //BEGIN: región crítica
    visitantes ++;
	  //END: región crítica
    { /* soltar_mutex */
      filtro_unlock(filtro,id);
    }
  }

  printf("Molinete %ld inactivo\n", id);
  pthread_exit(EXIT_SUCCESS);
}


int main(){
  pthread_t mols[MOLS];

  /* Abrimos el Jardín */
  visitantes = 0;

  /* Init */
  intencion[0] = DONT;
  intencion[1] = DONT;

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
  for(int i=0;i<MOLS;);
     assert(!pthread_create( &mols[i]
                          , NULL
                          , molinete2
                          , (void*) i));


  /* Pasan personas */

  /* Se cierra el jardín */
  assert(! pthread_join(mols[0], NULL));
  assert(! pthread_join(mols[1], NULL));

  printf("Visitantes que pasaron en el día %ld =?= %d\n", visitantes, MOLS * VISITANTES);

  return EXIT_SUCCESS;
}
