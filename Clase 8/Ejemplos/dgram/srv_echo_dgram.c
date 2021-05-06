#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "crear_socket_nombrado.h"
#include "serv_conf.h"

/* Servidor Echo */

int main(){
  int sock_srv;
  ssize_t nbytes;
  char buff[MAXMSG+1];
  struct sockaddr_un cliente;
  socklen_t size = sizeof(cliente);

  /* Creación de un socket nombrado  */
  sock_srv = crear_socket_nombrado(SRV_NOMBRE);
  printf("[DIAG] SOCKET: %s\n", SRV_NOMBRE);

  /* El servidor se queda a la espera de un mensaje por el socket */
  nbytes = recvfrom(sock_srv
                    , buff
                    , MAXMSG
                    , 0
                    , (struct sockaddr *) & cliente
                    , &size);
  if(nbytes < 0){
    perror("Falló el recvfrom");
    exit(EXIT_FAILURE);
  }
  buff[nbytes] = '\0';
  /**/
  printf("[DIAG] RECV: >%s<\n", buff);

  /* Responder el echo! */
  nbytes = sendto(sock_srv
                  , buff
                  , nbytes
                  , 0
                  , (struct sockaddr *) & cliente
                  , size);
  if(nbytes < 0){
    perror("Falló el sendto");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] SEND: OK\n");
  /*****************************************/
  rm_socket_nombrado(SRV_NOMBRE, sock_srv);

  return EXIT_SUCCESS;
}
