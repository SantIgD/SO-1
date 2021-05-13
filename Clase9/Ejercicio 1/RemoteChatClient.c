/* RemoteClient.c
   Se introducen las primitivas necesarias para establecer una conexión simple
   dentro del lenguaje C utilizando sockets.
*/
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netdb.h>
/**********/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>


char nombre[50];
char ip[100];
char puerto[100];

/*
  El archivo describe un sencillo cliente que se conecta al servidor establecido
  en el archivo RemoteServer.c. Se utiliza de la siguiente manera:
  $cliente IP port
 */

void error(char *msg){
  exit((perror(msg), 1));
}

int conectar_socket_red(char* ip,char* puerto,struct addrinfo* resultado){
  int sock;


  /* Inicializamos el socket */
  if( (sock = socket(AF_INET , SOCK_STREAM, 0)) < 0 )
    error("No se pudo iniciar el socket");

  /* Buscamos la dirección del hostname:port */
  if (getaddrinfo(ip, puerto, NULL, &resultado)){
    fprintf(stderr,"No se encontro el host: %s \n",ip);
    exit(2);
  }

  if(connect(sock, (struct sockaddr *) resultado->ai_addr, resultado->ai_addrlen) != 0)
    /* if(connect(sock, (struct sockaddr *) &servidor, sizeof(servidor)) != 0) */
    error("No se pudo conectar :(. ");



  return sock;
}

void* escribir(void * arg){

  int sock = *(int *) arg;
  char buf[1024];
  ssize_t bytes;

  /* Respondemos con nuestro Nombre! */
  send(sock, nombre, sizeof(nombre),0);

  while(1){

    scanf(" %[^\n]s",buf);
    send(sock,buf,sizeof(buf),0);

  }

}

void* recibir (void * arg){

  int sock = *(int *) arg;
  char buf[1024];
  ssize_t bytes;


  while(1){

    bytes = recv(sock, buf, sizeof(buf),0);

    buf[bytes] = '\0';
    
    printf("%s\n", buf);
  }

}

void upload_data(char* argv[], char* ip, char* puerto, char* nombre ){

  strncpy(nombre,argv[3],sizeof(nombre));
  strncpy(ip,argv[1],sizeof(ip)+1);
  strncpy(puerto,argv[2],sizeof(puerto));

}

int main(int argc, char **argv){

  char buf[1024];
  int sock;
  ssize_t bytes;
  struct addrinfo *resultado;

  pthread_t clienteEscribir, clienteRecibir;

  /*Chequeamos mínimamente que los argumentos fueron pasados*/
  if(argc != 4){
    fprintf(stderr,"El uso es \'%s IP port\'", argv[0]);
    exit(1);
  }

  upload_data(argv,ip,puerto,nombre);

  sock = conectar_socket_red(ip,puerto,resultado);

  printf("Se conecto con el server pa\n");
  
  pthread_create(&clienteEscribir , NULL , escribir, (void*) &sock);
  pthread_create(&clienteRecibir , NULL , recibir,(void*) &sock);

 /*Empezamos la comunicacion en el chat*/
  /* bucle infinito*/
  
  
  pthread_join(clienteRecibir,NULL);
  pthread_join(clienteEscribir,NULL);

   /* Cerramos :D!*/
  freeaddrinfo(resultado);
  close(sock);

  return 0;
}
