#include <stdio.h> /* Print */
#include <unistd.h> /* Read/Write + Fork */
#include <assert.h>
#include <stdlib.h>

#define BUFFER 1024

int main(){
  int fd[2];
  int p;
  char buffer[BUFFER] = "hola que tal";

  /* Creación del pipe */
  assert(! pipe(fd));

  /* close(0); */
  /* close(fd[0]); */

  /* while(1){ */
  /*   /\* printf("Holis\n"); *\/ */
  /*   write(1, "Holis\n", 6); */
  /*   sleep(1); */
  /* } */

  /* Fork */
  assert((p = fork()) >= 0);

  if (p == 0){ /* Child */
    ssize_t rd;
    /* Cerramos el extremo de escritura */
    close(fd[1]);

    /* Leemos el mensaje, bloqueante */
    rd = read(fd[0], buffer, BUFFER);
    buffer[rd] = 0;

    /* Printf cabeza */
    printf("Llegó: >%s<\n", buffer);

    /* Cerramos el extremo que falta */
    close(fd[0]);

    exit(EXIT_SUCCESS);

  } else { /* Parent */
    char buffP[BUFFER];
    /* Cerramos el extremo de lectura */
    close(fd[0]);

    /* while(1){ */
    /*   printf("Parent \n"); */
    /*   sleep(1); */
    /* } */
    read(0, buffP, BUFFER);

    /* Código muerto */

    /* Escribimos un mensaje al  */
    write(fd[1],"holis",5);

    /* Cerramos el extremo que falta */
    close(fd[1]);

    exit(EXIT_SUCCESS);
  }

  /* Código muerto */
  exit(EXIT_SUCCESS);
}
