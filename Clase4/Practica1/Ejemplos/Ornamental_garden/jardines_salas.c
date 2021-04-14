#include <pthread.h> /* PThread */
#include <stdio.h> /* Printf */
#include <stdlib.h>
#include <assert.h>
#include "filtro_salas.h"

#define MOLS 3
#define VISITANTES 100


/*creamos el filter*/
filtro_t* filter ;


/* Cantidad de visitantes en el Jardín */
long int visitantes;

void* molinete(void *arg){

  long int id = *(long int*) arg;

  printf("Molinete %ld activo\n",id);

  for(int i = 0; i < VISITANTES; i++){
    { /* tomar_mutex */
      filtro_lock(filter,id);
    }
	  //BEGIN: región crítica
    //if(filter->salas[id] == MOLS-2){ // acceder a seccion critica
        visitantes ++;
        printf("aca hilo [%ld]\n",id);
    //}else{
    //   i--;
    //}
        
	  //END: región crítica
    { /* soltar_mutex */
      filtro_unlock(filter,id);
    }
  }

  printf("Molinete %ld inactivo\n", id);
  pthread_exit(EXIT_SUCCESS);
}


int main(){
  pthread_t mols[MOLS];

  /* Abrimos el Jardín */
  visitantes = 0;

  filter = filtro(MOLS);

  int i=0;
  long int idth[MOLS];

  /* Habilitamos los molinetes */
  for(;i<MOLS;i++){
    idth[i]=i;
     assert(!pthread_create( &mols[i]
                          , NULL
                          , molinete
                          , (void*) &idth[i]));
  }

  /* Pasan personas */

  /* Se cierra el jardín */
  int j=0;
  for(;j<MOLS;j++)
    assert(! pthread_join(mols[j], NULL));
    

  printf("Visitantes que pasaron en el día %ld =?= %d\n", visitantes, MOLS * VISITANTES);
  filtro_delete(filter);

  return EXIT_SUCCESS;
}
