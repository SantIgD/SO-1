#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <pthread.h>
#include <assert.h>

#define CANT_RECEP 2
int MAXMSG = 0; 
/* Servidor Echo con Sockets de tipo Stream */

void* recepcionista(void* arg){
  int sock_cli;
  int sock_srv = *(int*)arg;
  struct sockaddr_un cli_nombre;
  socklen_t size;
  size = sizeof(struct sockaddr_un);
  ssize_t nbytes;
  char buff[MAXMSG+1];

  while(1){

    /* El servidor se pone a la espera de conexiones */
    if(listen(sock_srv, CANT_RECEP) < 0){
      perror("Falló el listen");
      exit(EXIT_FAILURE);
    }

    printf("[DIAG] Listen Ok :D\n");

    /* Apareció una conexión :D! */
    sock_cli = accept(sock_srv, (struct sockaddr *) &cli_nombre, & size);

    printf("[Cliente %d] Conexion establecida\n",sock_cli);
    if(sock_cli < 0){
      perror("Falló el 'accept' ");
      exit(EXIT_FAILURE);
    }

    printf("[DIAG] Acc >%s<:D\n",cli_nombre.sun_path);

    /* Recibimos un mensaje */
    nbytes = recv(sock_cli, buff, MAXMSG, 0);
    buff[nbytes] = '\0';
    printf("[DIAG]: Llegó >%s<\n", buff);

    strcpy(buff,"Recibido!!!\0");
    /* Enviamos el echo */
    nbytes = send(sock_cli, buff, nbytes, 0);
    if(nbytes < 0 ){
      perror("Falló el envío del echo");
      exit(EXIT_FAILURE);
    }

    printf("[DIAG] Novimo\n");
    close(sock_cli);
    /*******/
  }


  pthread_exit(EXIT_SUCCESS);
}


int main(int argc,char *argv[]){

  if (argc != 3){
    
    /* Argv[0]= name programa
      ,Argv[1]= Server Name
      ,Argv[2]= MAXMSG  */

    perror("Argumentos inválidos");
    exit(EXIT_FAILURE);
  }
  
  int sock_srv;
  struct sockaddr_un srv_nombre;
  socklen_t size;
  MAXMSG = atoi(argv[2]);
  char* SRV_NOMBRE = argv[1];
  pthread_t recepcionistas[CANT_RECEP];

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

  
  /* Creacion de recepcionistas*/
  for(int i=0; i < CANT_RECEP; i++){

      /* Habilitamos a los recepcionistas */
      assert(!pthread_create( &recepcionistas[i]
                              , NULL
                              , recepcionista
                              , (void*) &sock_srv));
    
  }    
        
  /* Esperar a que terminen */
  for(int i=0; i < CANT_RECEP; i++)
      assert(! pthread_join(recepcionistas[i], NULL));



  /* Servidor cerrándose */
  close(sock_srv);
  remove(SRV_NOMBRE);
}
