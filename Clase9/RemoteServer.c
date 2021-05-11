/* RemoteServer.c
   Se introducen las primitivas necesarias para establecer una conexión simple
   dentro del lenguaje C utilizando sockets.
 */
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netinet/in.h>
/**********/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>

#define LISTEN_MAX 5

/*
 * En la cabecera que incluimos `<netinet/in.h>`, una dirección sockaddr_in se
 * define como:
 * struct sockaddr_in {
 *      short          sin_family;
 *      u_short        sin_port;
 *      struct in_addr sin_addr;
 *      char           sin_zero[8];
 * };
 *
 */

/*
 * El servidor va a tomar el puerto como argumento.
 */

void error(char *msg){
  exit((perror(msg), 1));
}

int main(int argc, char **argv){
  int sock, soclient;
  char buf[1024];
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;

  if (argc <= 1) error("Faltan argumentos");

  /* Creamos el socket */
  if( (sock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    error("Socket Init");

  /* Creamos a la dirección del servidor.*/
  servidor.sin_family = AF_INET; /* Internet */
  servidor.sin_addr.s_addr = INADDR_ANY; /**/
  servidor.sin_port = htons(atoi(argv[1]));

  /* Inicializamos el socket */
  if (bind(sock, (struct sockaddr *) &servidor, sizeof(servidor)))
    error("Error en el bind");

  printf("Binding successful, and listening on %s\n",argv[1]);

  /* Ya podemos aceptar conexiones */
  if(listen(sock, LISTEN_MAX) == -1){
    perror(" Listen error ");
    exit(1);
  }

  /* Now we can accept connections as they come*/
  clientelen = sizeof(clientedir);
  if ((soclient = accept(sock, (struct sockaddr *) &clientedir
                         , &clientelen)) == -1){
    perror("Accepting error");
    exit(1);
  }
  /*Connection Successful*/
  printf("Connected!\n");
  /* SEND PING! */
  send(soclient, "PING!", sizeof("PING!"), 0);
  /* WAIT FOR PONG! */
  recv(soclient, buf, sizeof(buf), 0);
  printf("Recv: %s\n", buf);

  close(sock);

  return 0;
}
