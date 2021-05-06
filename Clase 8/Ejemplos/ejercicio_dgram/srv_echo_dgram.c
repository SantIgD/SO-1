#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "crear_socket_nombrado.h"
#include <signal.h>

int socketCreado = 0;
char * srvNombre;

void handler(int arg){

  if (socketCreado){ 

    rm_socket_nombrado(srvNombre, socketCreado);
  }

  exit(EXIT_FAILURE);
}


/* Servidor Echo */

int main(int argc, char* argv[]){

  if (argc != 3){
    
    /* Argv[0]= name programa
      ,Argv[1]= Server Name
      ,Argv[2]= MAXMSG  */

    perror("Argumentos inválidos");
    exit(EXIT_FAILURE);
  }

  signal(SIGTERM, handler);


  int MAXMSG = atoi(argv[2]);
  int socketCreado;
  ssize_t nbytes;
  char buff[MAXMSG+1];
  struct sockaddr_un cliente;
  socklen_t size = sizeof(cliente);

  srvNombre = argv[1];

  /* Creación de un socket nombrado  */
  socketCreado = crear_socket_nombrado(srvNombre);
  printf("[DIAG] SOCKET: %s\n", srvNombre);

  /* El servidor se queda a la espera de un mensaje por el socket */
  nbytes = recvfrom(socketCreado
                    , buff
                    , MAXMSG
                    , 0
                    , (struct sockaddr *) & cliente
                    , &size);
  if(nbytes < 0){
    perror("Falló el recvfrom");
    rm_socket_nombrado(srvNombre, socketCreado);
    exit(EXIT_FAILURE);
  }
  buff[nbytes] = '\0';
  /**/
  printf("[DIAG] RECV: >%s<\n", buff);

  /* Responder el echo! */
  nbytes = sendto(socketCreado
                  , buff
                  , nbytes
                  , 0
                  , (struct sockaddr *) & cliente
                  , size);
  if(nbytes < 0){
    perror("Falló el sendto");
    rm_socket_nombrado(srvNombre, socketCreado);
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] SEND: OK\n");
  /*****************************************/
  rm_socket_nombrado(srvNombre, socketCreado);

  return EXIT_SUCCESS;
}
