#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "crear_socket_nombrado.h"
#include <signal.h>

/*Mensaje a enviar y nombre del socket del cliente */
#define CLI_NOMBRE "./CLIENTE"



#define LECTURA 0
#define ESCRITURA 1

/* Bandera */
int socketCreado = 0;


void nbytes_error_handler(ssize_t nbytes,int mode){

  if(nbytes < 0){
    
    switch(mode){
      case ESCRITURA:
      {
        perror("Falló la escritura");
        break;
      }
      default: // LECTURA
      {
        perror("Falló la lectura");
      }
    }

    /*Eliminamos el socket y salimos*/  
    rm_socket_nombrado(CLI_NOMBRE, socketCreado);
    exit(EXIT_FAILURE);  
  }
  
}


void handler(int arg){

  if (socketCreado){ 

    rm_socket_nombrado(CLI_NOMBRE, socketCreado);
  }

  exit(EXIT_FAILURE);
}

/* Cliente */

int main(int argc, char* argv[]){

  /*verificamos que la cantidad de argumentos ingresados sea la correcta*/
  if (argc != 4){
    
    /* Argv[0]= name programa
      ,Argv[1]= Server Name
      ,Argv[2]= bufferSize 
      ,Argv[3]= mensaje */

    perror("Argumentos inválidos");
    exit(EXIT_FAILURE);
  }

  /* Manejo de señales*/
  signal(SIGINT, handler);
  signal(SIGTERM, handler);


  
   /* Almacenamos los argumentos */
  char* srvNombre = argv[1];
  int bufferSize = atoi(argv[2]);
  char* msg = argv[3];

  /* Se utiliza para saber la cantidad de bytes leidos/escritos */
  ssize_t nbytes; 
  
  /*Se utiliza para almacenar la cadena que se lee desde un cliente o se envia desde el servidor*/
  char buff[bufferSize+1]; 
  
  /* Se utiliza para almacenar la direccion del servidor*/
  struct sockaddr_un srv;
  socklen_t size = sizeof(srv);


  /* Creación del socket asociandolo con el nombre del cliente  */
  socketCreado = crear_socket_nombrado(CLI_NOMBRE);
  printf("[DIAG] SOCKET: %s\n", CLI_NOMBRE);

  /* Nombre/Dirección del servidor */
  srv.sun_family = AF_UNIX; // modo local
  strncpy(srv.sun_path
         , srvNombre
         , sizeof( srv.sun_path ));

  /* Llenamos el buffer con el mensaje a enviar */
  strcpy(buff, msg);


  /* Enviar el mensaje al servidor! */
  nbytes = sendto(socketCreado
                  , buff
                  , strlen(buff)+1
                  , 0
                  , (struct sockaddr *) & srv
                  , size);

  /*Verificamos que la escritura se haya realizado con exito*/                  
  nbytes_error_handler(nbytes,ESCRITURA);
  printf("[DIAG] SEND: OK\n");

  /*Esperamos la respuesta del servidor */
  nbytes = recvfrom(socketCreado
                    , buff
                    , bufferSize
                    , 0
                    , (struct sockaddr *) & srv
                    , &size);

   /*Verificamos que la lectura se haya realizado con exito*/                  
  nbytes_error_handler(nbytes,LECTURA);

  /*Imprimimos el mensaje*/
  buff[nbytes] = '\0';
  printf("[DIAG] RECV: >%s<\n", buff);

  /*****************************************/
  /*Eliminamos el socket*/
  rm_socket_nombrado(CLI_NOMBRE, socketCreado);

  return EXIT_SUCCESS;
}
