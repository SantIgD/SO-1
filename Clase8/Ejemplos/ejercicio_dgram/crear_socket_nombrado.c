#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h> /* strncpy */
#include "crear_socket_nombrado.h"


int archivoCreado = 0;


int crear_socket_nombrado(const char * archivo){

  /* Variable donde se va a almacenar la representacion del socket como fd*/
  int sock;

  /* Estructura para darle una direccion con nombre al socket*/
  struct sockaddr_un nombre;
  socklen_t size;

  /* Creación del socket */
  sock = socket(AF_UNIX, SOCK_DGRAM, 0);

  if(sock < 0){
    perror("Falló la creación del socket ");
    exit(EXIT_FAILURE);
  }

  /* Creamos la estructura de la dirección del socket */
  nombre.sun_family = AF_UNIX; // modo local
  strncpy(nombre.sun_path, archivo, sizeof(nombre.sun_path)); // nombramos al socket como archivo
  size = sizeof(nombre);

  /* Asignación del nombre */
  if( (bind(sock, (struct sockaddr*) & nombre , size)) < 0 ){
    perror("Falló la asignación de nombre");
    rm_socket_nombrado(archivo,sock);
    exit(EXIT_FAILURE);
  }

  archivoCreado ++;

  return sock;
}


int rm_socket_nombrado(const char * archivo, int socket){
  
  int ret;
  
  close(socket);

  if (archivoCreado){
    ret = remove(archivo);
    archivoCreado--;
  }

  return ret;
}