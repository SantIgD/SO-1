#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "crear_socket_nombrado.h"
#include "serv_conf.h"

#define CLI_NOMBRE "./CLIENTE"
#define MSG "JAIME!"

/* Cliente Echo */

int main(){
  int sock_cli;
  ssize_t nbytes;
  char buff[MAXMSG+1];
  struct sockaddr_un srv;
  socklen_t size = sizeof(srv);

  /* Creación de un socket nombrado  */
  sock_cli = crear_socket_nombrado(CLI_NOMBRE);
  printf("[DIAG] SOCKET: %s\n", CLI_NOMBRE);

  /* Nombre/Dirección del servidor */
  srv.sun_family = AF_UNIX;
  strncpy(srv.sun_path
         , SRV_NOMBRE
         , sizeof( srv.sun_path ));

  /* Llenamos el buffer */
  strcpy(buff, MSG);

  /* Enviar 'buff' al servidor! */
  nbytes = sendto(sock_cli
                  , buff
                  , strlen(buff)+1
                  , 0
                  , (struct sockaddr *) & srv
                  , size);
  if(nbytes < 0){
    perror("Falló el sendto");
    exit(EXIT_FAILURE);
  }
  printf("[DIAG] SEND: OK\n");

  /* Nos quedamos entonces a la espera de la respuesta del servidor */
  nbytes = recvfrom(sock_cli
                    , buff
                    , MAXMSG
                    , 0
                    , (struct sockaddr *) & srv
                    , &size);
  if(nbytes < 0){
    perror("Falló el recvfrom");
    exit(EXIT_FAILURE);
  }
  buff[nbytes] = '\0';
  /**/
  printf("[DIAG] RECV: >%s<\n", buff);

  /*****************************************/
  rm_socket_nombrado(CLI_NOMBRE, sock_cli);

  return EXIT_SUCCESS;
}
