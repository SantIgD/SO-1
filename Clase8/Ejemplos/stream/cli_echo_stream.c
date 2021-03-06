#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "serv_conf.h"

#define MSG "JAIME!"

int main(){
  int sock_cli;
  struct sockaddr_un srv_nombre;
  socklen_t size;
  ssize_t nbytes;
  char buff[MAXMSG+1];

  sock_cli = socket(AF_UNIX, SOCK_STREAM, 0);
  if(sock_cli < 0){
    perror("Falló la creación del socket");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] Se creó el socket :D\n");

  /* Asignamos la dirección del servidor */
  srv_nombre.sun_family = AF_UNIX;
  strncpy(srv_nombre.sun_path, SRV_NOMBRE,sizeof(srv_nombre.sun_path));
  size = sizeof(struct sockaddr_un);
  /******************************************/

  if((connect(sock_cli, (struct sockaddr *) &srv_nombre, size))< 0){
    perror("Falló el intento de conexión con el servidor");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] Conexión con %s OK!\n", SRV_NOMBRE);

  /* Enviamos un mensaje al Servidor */
  nbytes = send(sock_cli, MSG,sizeof(MSG), 0);
  if(nbytes < 0 ){
    perror("Falló el envío del mensaje");
    exit(EXIT_FAILURE);
  }

  /* Esperamos la respuesta */
  nbytes = recv(sock_cli, buff, MAXMSG, 0);
  if(nbytes < 0){
    perror("Falló la recepción de un mensaje");
    exit(EXIT_FAILURE);
  }
  buff[nbytes] = '\0';
  printf("[DIAG] Llegó >%s<\n", buff);

  close(sock_cli);
  return(EXIT_SUCCESS);
}
