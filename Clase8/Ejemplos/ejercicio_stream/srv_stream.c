#include <sys/socket.h>
#include <sys/un.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#define BLOG 1
#define ESCRITURA 1
#define LECTURA 0


/*Nombre del servidor*/
char* srvName;

/*sockets del server y el cliente */
int sock_srv, sock_cli, socketCreado=0;

/* Servidor Echo con Sockets de tipo Stream */

void rm_sockets(){
 
 if (socketCreado){ 
    close(sock_srv);
    remove(srvName);
    socketCreado--;
    if (socketCreado)
      close(sock_cli);
  }
}

void handler(int arg){

  rm_sockets();

  exit(EXIT_FAILURE);
}


void socket_creation_error_handler(int sock){

  if(sock < 0){
    perror("Falló la creación del socket");
    exit(EXIT_FAILURE);
  }

}

void socket_assing_address(struct sockaddr_un* socket_nombre, char* name){
  
  socket_nombre->sun_family = AF_UNIX;
  //printf("[Nombre del server] = %s\n",name);
  strncpy(socket_nombre->sun_path, name,sizeof(socket_nombre->sun_path));

}

void bind_error_handler(int bindResult){


  if(bindResult){
    perror("Falló la asignación del nombre del servidor");
    close(sock_srv);
    exit(EXIT_FAILURE);
  }

}

void listen_error_handler(int listenResult){

  if(listenResult < 0){
      perror("Falló el listen");
      rm_sockets();
      exit(EXIT_FAILURE);
    }
}

void acept_error_handler(int sockAccepted){
  
  if(sockAccepted < 0){
      perror("Falló el 'accept' ");
      rm_sockets();
      exit(EXIT_FAILURE);
    }

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
    rm_sockets();
    exit(EXIT_FAILURE);  
  }
  
}

int main(int argc,char *argv[]){

   /*verificamos que la cantidad de argumentos ingresados sea la correcta*/
  if (argc != 3){
    
    /* Argv[0]= name programa
      ,Argv[1]= Server Name
      ,Argv[2]= bufferSize  */

    perror("Argumentos inválidos");
    exit(EXIT_FAILURE);
  }

 
  signal(SIGINT, handler);
  signal(SIGTERM, handler);

  /* Se utiliza para almacenar la direccion del cliente y la del servidor*/
  struct sockaddr_un srv_nombre, cli_nombre;
  socklen_t size;

  /* Se utiliza para saber la cantidad de bytes leidos/escritos */
  ssize_t nbytes;

  /* Almacenamos los argumentos */
  int bufferSize = atoi(argv[2]);
  srvName = argv[1];

  /*Se utiliza para almacenar la cadena que se lee desde un cliente o se envia desde el servidor*/
  char buff[bufferSize+1];

  /* Creación de Socket Servidor */
  sock_srv = socket(AF_UNIX, SOCK_STREAM, 0);

  /*Verificamos que la creacion del socket se haya raelizado con exito*/
  socket_creation_error_handler(sock_srv);

  printf("[DIAG] SOCKET Ok :D\n");

  /* Asignamos la dirección del servidor */
  socket_assing_address(&srv_nombre,srvName);
  size = sizeof(struct sockaddr_un);
  
  /*Nombramos al socket del servidor*/
  
  bind_error_handler( bind(sock_srv, (struct sockaddr *) &srv_nombre,sizeof(srv_nombre.sun_path)));
  socketCreado++;

  printf("[DIAG] BIND Ok :D\n");

  while(1){

    /* El servidor se pone a la espera de conexiones */
    listen_error_handler(listen(sock_srv, BLOG));

    printf("[DIAG] Listen Ok :D\n");

    /* Apareció una conexión :D! */
    sock_cli = accept(sock_srv, (struct sockaddr *) &cli_nombre, &size);
    
    acept_error_handler(sock_cli);
    
    socketCreado++;
    
    printf("[DIAG] Acc >%s<:D\n",cli_nombre.sun_path);

    /* Recibimos un mensaje */
    nbytes = recv(sock_cli, buff, bufferSize, 0);

    /*Verificamos que la lectura se haya realizado con exito*/                  
    nbytes_error_handler(nbytes,LECTURA);

    /*Impremimos el mensaje*/
    buff[nbytes] = '\0';
    printf("[DIAG]: Llegó >%s<\n", buff);

    /*Escribimos el mensaje que el cliente va a recibir*/
    strcpy(buff,"Recibido!!!\0");
    
    
    /* Enviamos el mensaje al cliente*/
    nbytes = send(sock_cli, buff, nbytes, 0);
    
    /*Verificamos que la lectura se haya realizado con exito*/                  
    nbytes_error_handler(nbytes,ESCRITURA);

    printf("[DIAG] Novimo\n");
    close(sock_cli);
    /*******/
  }

  /* Servidor cerrándose */
  close(sock_srv);
  remove(srvName);
}
