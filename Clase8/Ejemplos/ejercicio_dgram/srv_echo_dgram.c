#include <stdlib.h>
#include <stdio.h>
#include <sys/socket.h>
#include <sys/un.h>
#include "crear_socket_nombrado.h"
#include <signal.h>

#define LECTURA 0
#define ESCRITURA 1

/* Bandera */
int socketCreado = 0;

/*Nombre del servidor, la definimos global para utilizarla en el handler*/
char * srvNombre;

void handler(int arg){

  if (socketCreado){ 

    rm_socket_nombrado(srvNombre, socketCreado);
  }

  exit(EXIT_FAILURE);
}

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
    rm_socket_nombrado(srvNombre, socketCreado);
    exit(EXIT_FAILURE);  
  }
  
}

/* Servidor */

int main(int argc, char* argv[]){

  /*verificamos que la cantidad de argumentos ingresados sea la correcta*/
  if (argc != 3){
    
    /* Argv[0]= name programa
      ,Argv[1]= Server Name
      ,Argv[2]= bufferSize  */

    perror("Argumentos inválidos");
    exit(EXIT_FAILURE);
  }

  /* Manejo de señales*/
  signal(SIGINT, handler);
  signal(SIGTERM, handler);

  
  /* Almacenamos los argumentos */
  int bufferSize = atoi(argv[2]);
  
  srvNombre = argv[1];


  /* Se utiliza para saber la cantidad de bytes leidos/escritos */
  ssize_t nbytes; 
  
  /*Se utiliza para almacenar la cadena que se lee desde un cliente o se envia desde el servidor*/
  char buff[bufferSize+1]; 
  
  /* Se utiliza para almacenar la direccion del cliente*/
  struct sockaddr_un cliente;
  socklen_t clienteSize = sizeof(cliente);

  

  /* Creación del socket asociado al nombre del servidor  */
  socketCreado = crear_socket_nombrado(srvNombre);
  printf("[DIAG-SERVER] SOCKET: %s\n", srvNombre);

  while(1){
    /* El servidor se queda a la espera de un mensaje de algun cliente por el socket */
    nbytes = recvfrom(socketCreado
                      , buff
                      , bufferSize
                      , 0
                      , (struct sockaddr *) & cliente
                      , &clienteSize);
                      
    /*Verificamos que la lectura se haya realizado con exito*/                  
    nbytes_error_handler(nbytes,LECTURA);


    /*Imprimimos el mensaje*/
    buff[nbytes] = '\0';
    printf("[DIAG-SERVER] RECV: >%s<\n", buff);

    strncpy(buff,"ACK",4);
    /* Respondemos al cliente */
    nbytes = sendto(socketCreado
                    , buff
                    , nbytes
                    , 0
                    , (struct sockaddr *) & cliente
                    , clienteSize);


    /*Verificamos que la escritura se haya realizado con exito*/                  
    nbytes_error_handler(nbytes,ESCRITURA);
    printf("[DIAG-SERVER] SEND: OK\n");

  }
  
  /*****************************************/
  /*Eliminamos el socket*/
  rm_socket_nombrado(srvNombre, socketCreado);

  return EXIT_SUCCESS;
}
