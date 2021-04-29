#include "pipeBidireccional.h"  
#include <stdio.h> /* Print */
#include <unistd.h> /* Read/Write + Fork */
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#define BUFFER 1024

struct _pipe{  
  
  int CtoP[2];
  int PtoC[2];
  char buffC[BUFFER];
  char buffP[BUFFER];
};


pipe_b* pipe_b_create(){

  return malloc(sizeof(pipe_b));

}

void pipe_b_init(pipe_b* pip){

  assert(! pipe(pip->CtoP));
  assert(! pipe(pip->PtoC));

}

void pipe_b_child_write(pipe_b* pip, char* str){
  
  //str = strcat("[C]",str);
  /* Escribimos un mensaje al  padre*/
  write(pip->CtoP[1],str,strlen(str));
}

void pipe_b_parent_write(pipe_b* pip, char* str){

  //str = strcat("[P]",str);

  /* Escribimos un mensaje al  hijo*/
    write(pip->PtoC[1],str,strlen(str));
}

void pipe_b_parent_read(pipe_b* pip){

  ssize_t rd;
/* Leemos el mensaje del padre, bloqueante */
  rd = read(pip->CtoP[0], pip->buffC, BUFFER);
  pip->buffC[rd] = 0;

  /* Printf cabeza */
  printf("Llegó al padre: >%s<\n", pip->buffC);

}

void pipe_b_child_read(pipe_b* pip){
  
  ssize_t rd;
  /* Leemos el mensaje del padre, bloqueante */
  rd = read(pip->PtoC[0], pip->buffP, BUFFER);
  pip->buffP[rd] = 0;

  /* Printf cabeza */
  printf("Llegó al hijo: >%s<\n", pip->buffP);

}

void pipe_b_parent_close_end(pipe_b* pip){

 
  /* Cerramos el extremo de escritura del fd de lectura del parent*/
  close(pip->PtoC[1]);
}

void pipe_b_child_close_end(pipe_b* pip){

     /* Cerramos el extremo de escritura del fd de escritura del child*/
    close(pip->CtoP[1]);
}

void pipe_b_close_begin_child(pipe_b* pip){

  /* Cerramos el extremo de lectura del fd de escritura del child*/
  close(pip->CtoP[0]);

}

void pipe_b_close_begin_parent(pipe_b* pip){

  /* Cerramos el extremo de lectura del fd de lectura del parent*/
  close(pip->PtoC[0]);

}

void pipe_b_destroy(pipe_b* pipe){
  free(pipe);
}