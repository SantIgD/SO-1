#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "crear_socket_nombrado.h"
#include <signal.h>

#define CLI_NOMBRE "./CLIENTE"
#define MSG "JAIME!"

int socketCreado = 0;


void handler(int arg){

  if (socketCreado){ 

    rm_socket_nombrado(CLI_NOMBRE, socketCreado);
  }

  exit(EXIT_FAILURE);
}

/* Cliente Echo */

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
  char * srvNombre = argv[1];
  int socketCreado;
  ssize_t nbytes;
  char buff[MAXMSG+1];
  struct sockaddr_un srv;
  socklen_t size = sizeof(srv);

  /* Creación de un socket nombrado  */
  socketCreado = crear_socket_nombrado(CLI_NOMBRE);
  printf("[DIAG] SOCKET: %s\n", CLI_NOMBRE);

  /* Nombre/Dirección del servidor */
  srv.sun_family = AF_UNIX;
  strncpy(srv.sun_path
         , srvNombre
         , sizeof( srv.sun_path ));

  /* Llenamos el buffer */
  strcpy(buff, MSG);

  /* Enviar 'buff' al servidor! */
  nbytes = sendto(socketCreado
                  , buff
                  , strlen(buff)+1
                  , 0
                  , (struct sockaddr *) & srv
                  , size);

  if(nbytes < 0){
    perror("Falló el sendto");
    rm_socket_nombrado(CLI_NOMBRE, socketCreado);
    exit(EXIT_FAILURE);
  }
  printf("[DIAG] SEND: OK\n");

  /* Nos quedamos entonces a la espera de la respuesta del servidor */
  nbytes = recvfrom(socketCreado
                    , buff
                    , MAXMSG
                    , 0
                    , (struct sockaddr *) & srv
                    , &size);
  if(nbytes < 0){
    perror("Falló el recvfrom");
    rm_socket_nombrado(CLI_NOMBRE, socketCreado);
    exit(EXIT_FAILURE);
  }
  buff[nbytes] = '\0';
  /**/
  printf("[DIAG] RECV: >%s<\n", buff);

  /*****************************************/
  rm_socket_nombrado(CLI_NOMBRE, socketCreado);

  return EXIT_SUCCESS;
}
