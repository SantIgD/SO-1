#include <stdio.h> /* Print */
#include <unistd.h> /* Read/Write + Fork */
#include <assert.h>
#include <stdlib.h>

#define BUFFER 1024

int main(){
  int fd[2];
  int p;
  

  /* Creación del pipe */
  assert(! pipe(fd));


  /* Fork */
  assert((p = fork()) >= 0);


  if (p == 0){ /* Child */
    ssize_t rd;
    char buffC[BUFFER];
    //char buffer[BUFFER] = "hola que tal";


    /* Escribimos un mensaje al  padre*/
    write(fd[1],"[C] Hola pa!",12);

    /* Leemos el mensaje del padre, bloqueante */
    rd = read(fd[0], buffC, BUFFER);
    buffC[rd] = 0;

    /* Printf cabeza */
    printf("Llegó al hijo: >%s<\n", buffC);

    exit(EXIT_SUCCESS);

  } else { /* Parent */
    char buffP[BUFFER];
    ssize_t rd;

    rd = read(fd[0], buffP, BUFFER);
    buffP[rd] = 0;

    /* Printf cabeza */
    printf("Llegó al padre: >%s<\n", buffP);


    /* Escribimos un mensaje al  */
    write(fd[1],"[P] Hola hijo, ¿Como estas?",28);

    /* Leemos el mensaje del hijo, bloqueante */
   
    
    /* Cerramos el extremo que falta */
  

    exit(EXIT_SUCCESS);
  }

  /* Código muerto */
  exit(EXIT_SUCCESS);
}


/*
  Usando el mismo canal de comunicacion ocurre que uno lee lo que escribe y no se quedan bloquedos esperando por
  lo que si las operaciones estan en el mismo orden cada uno lee lo que escribe y si estan invertidas uno lee lo que escribe 
  mientras el otro se queda esperando infinitamnete ya que no hay nada que leer
*/
