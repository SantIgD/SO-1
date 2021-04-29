#iinclude <stdio.h> /* Print */
#include <unistd.h> /* Read/Write + Fork */
#include <assert.h>
#include <stdlib.h>

#define BUFFER 1024

int main(){
  int fdLeeHijo[2],fdLeePadre[2];
  int p;
  

  /* Creación del pipe */
  assert(! pipe(fdLeeHijo));
  assert(! pipe(fdLeePadre));

  /* Fork */
  assert((p = fork()) >= 0);


  if (p == 0){ /* Child */
    ssize_t rd;
    char buffCSTDIN[BUFFER];
    char buffC[BUFFER];
    //char buffer[BUFFER] = "hola que tal";

    /* Cerramos el extremo de escritura del fd de lectura*/
    close(fdLeeHijo[1]);

    /* Cerramos el extremo de lectura del fd de escritura*/
    close(fdLeePadre[0]);

    
    //read(0,buffCSTDIN, BUFFER);

    /* Escribimos un mensaje al  padre*/
    write(fdLeePadre[1],"[C] Hola pa!",12);

    /* Leemos el mensaje del padre, bloqueante */
    rd = read(fdLeeHijo[0], buffC, BUFFER);
    buffC[rd] = 0;

    /* Printf cabeza */
    printf("Llegó al hijo: >%s<\n", buffC);

    /* Cerramos el extremo que falta */
    close(fdLeeHijo[0]);

    /* Cerramos el extremo que falta */
    close(fdLeePadre[1]);

    exit(EXIT_SUCCESS);

  } else { /* Parent */
    char buffP[BUFFER];
    char buffPSTDIN[BUFFER];
    ssize_t rd;

    /* Cerramos el extremo de lectura del hijo*/
    close(fdLeeHijo[0]);
    /* Cerramos el extremo de escritura del Padre*/
    close(fdLeePadre[1]);


    //read(0, buffPSTDIN, BUFFER);



    /* Escribimos un mensaje al  */
    write(fdLeeHijo[1],"[P] Hola hijo, ¿Como estas?",28);

    /* Leemos el mensaje del hijo, bloqueante */
    rd = read(fdLeePadre[0], buffP, BUFFER);
    buffP[rd] = 0;

    /* Printf cabeza */
    printf("Llegó al padre: >%s<\n", buffP);


    /* Cerramos el extremo que falta */
    close(fdLeeHijo[1]);
    close(fdLeePadre[0]);

    exit(EXIT_SUCCESS);
  }

  /* Código muerto */
  exit(EXIT_SUCCESS);
}
