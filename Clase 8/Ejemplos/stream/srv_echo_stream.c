#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "serv_conf.h"

#define BLOG 1

/* Servidor Echo con Sockets de tipo Stream */

int main(){
  int sock_srv, sock_cli;
  struct sockaddr_un srv_nombre, cli_nombre;
  socklen_t size;
  ssize_t nbytes;
  char buff[MAXMSG+1];

  /* Creación de Socket Servidor */
  sock_srv = socket(AF_UNIX, SOCK_STREAM, 0);
  if(sock_srv < 0){
    perror("Falló la creación del socket");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] SOCKET Ok :D\n");

  /* Asignamos la dirección del servidor */
  srv_nombre.sun_family = AF_UNIX;
  strncpy(srv_nombre.sun_path, SRV_NOMBRE,sizeof(srv_nombre.sun_path));
  size = sizeof(struct sockaddr_un);

  if(bind(sock_srv, (struct sockaddr *) &srv_nombre, size)){
    perror("Falló la asignación del nombre del servidor");
    exit(EXIT_FAILURE);
  }
  printf("[DIAG] BIND Ok :D\n");

  /* El servidor se pone a la espera de conexiones */
  if(listen(sock_srv, BLOG) < 0){
    perror("Falló el listen");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] Listen Ok :D\n");

  /* Apareció una conexión :D! */
  sock_cli = accept(sock_srv, (struct sockaddr *) &cli_nombre, & size);
  if(sock_cli < 0){
    perror("Falló el 'accept' ");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] Acc >%s<:D\n",cli_nombre.sun_path);

  /* Recibimos un mensaje */
  nbytes = recv(sock_cli, buff, MAXMSG, 0);
  buff[nbytes] = '\0';
  printf("[DIAG]: Llegó >%s<\n", buff);

  /* Enviamos el echo */
  nbytes = send(sock_cli, buff, nbytes, 0);
  if(nbytes < 0 ){
    perror("Falló el envío del echo");
    exit(EXIT_FAILURE);
  }

  printf("[DIAG] Novimo\n");
  close(sock_cli);
  /*******/

  /* Servidor cerrándose */
  close(sock_srv);
  remove(SRV_NOMBRE);
}
