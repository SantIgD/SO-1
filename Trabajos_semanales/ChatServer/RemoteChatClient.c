/*****************************/
/* RemoteClient.c */
/*****************************/

/* Cabeceras de Sockets */
/*****************************/
#include <sys/types.h>
#include <sys/socket.h>
/*****************************/

/* Cabecera de direcciones por red */
/*****************************/
#include <netdb.h>
/*****************************/


/*Librerias complementarias*/
/*****************************/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
/*****************************/


/* Threads! */
/*****************************/
#include <pthread.h>
/*****************************/

/* Signals */
/*****************************/
#include <signal.h>
/*****************************/

/*****Constates*****/
/**************************************************************/

#define MAXSIZEMSG 1024
#define MAXIP 25
#define MAXPORT 10
#define EXITMSG "/exit"
/**************************************************************/

/*Varibles globales*/
/**************************************************************/

char ip[MAXIP];
char puerto[MAXPORT];
int sock;//Socket del cliente que se conectara con el servidor
struct addrinfo *resultado;
int estado=-1;//bandera para el control de señales
pthread_t clienteEscribir, clienteRecibir;
/**************************************************************/

/*Funciones del cliente*/
/**************************************************************/

/*Printea error con el mensaje msg por pantalla*/
void error(char *msg);

/* Desconexion y cierre del cliente */
void cerrar_cliente();

/* Manejo de signals */
void handler(int arg);

/* Inicializa y conecta un socket a un ip:puerto*/
int conectar_socket_red();

/* Hilo para mandar mensajes al servidor */
void* escribir(void * arg);

/* Hilo para recibir mensajes del servidor */
void* recibir (void * arg);

/* Carga los argumentos recibidos por entrada estandar a variables globales*/
void upload_data(char* argv[]);

/* Creacion/Join de hilos */
void hilos();

/**************************************************************/


int main(int argc, char **argv){

  //Chequeamos mínimamente que los argumentos fueron pasados/
  
  if(argc != 3){
    fprintf(stderr,"La cantidad de argumentos no es correcta.\n");
    fprintf(stderr,"El uso es \'%s IP port\'", argv[0]);  
    exit(1);
  }
  
  /*Peparamos el manejo de señales*/  
  signal(SIGINT, handler);
  signal(SIGTERM, handler);
  
  /*Cargamos la informacion de los argumentos*/
  upload_data(argv);
       
  /*Conectamos con el servidor*/
  sock = conectar_socket_red();

  printf("Se ha podido establecer conexion con el server %s:%s\n",ip,puerto);
  
  /*Ponemos en funcionamiento la lectura y escritura*/
  hilos();

   /* Cerramos :D!*/
  freeaddrinfo(resultado);
  close(sock);
  return 0;
}

void salir_hilo(int arg){
  pthread_exit(NULL);
}

void* escribir(void * arg){

  int sock = *(int *) arg;
  char buf[MAXSIZEMSG];
  ssize_t bytes;
  signal(1,salir_hilo);
  while(1){

    scanf(" %[^\n]s",buf);
    send(sock,buf,sizeof(buf),0);

  }

}

void* recibir (void * arg){

  int sock = *(int *) arg,
      continuar=1;
  char buf[MAXSIZEMSG];
  ssize_t bytes;


  while(continuar){

    bytes = recv(sock, buf, sizeof(buf),0);
    
    if(strcmp(buf,"OK") == 0){
      continuar=0;
    }
    if(strcmp(buf,"shutdown") == 0){
      strcpy(buf,EXITMSG);
      send(sock,buf,sizeof(buf),0);
    }
    buf[bytes] = '\0';
    
    printf("%s\n", buf);
  }
  pthread_kill(clienteEscribir,1);
  pthread_exit(NULL);
}

void error(char *msg){
  exit((perror(msg), 1));
}


void cerrar_cliente(){
    
    if(estado==0){
        close(sock);
    }
    else if (estado==1){
        close(sock);
        freeaddrinfo(resultado);
    }
    error("Finalizo la conexion con el server");
}

void handler(int arg){
  char buf[MAXSIZEMSG]="/exit";
  send(sock,buf,sizeof(buf),0);
}


int conectar_socket_red(){
  int socksrv;


  /* Inicializamos el socket */
  if( (socksrv = socket(AF_INET , SOCK_STREAM, 0)) < 0 )
    error("No se pudo iniciar el socket");
    
  estado++;  
  /* Buscamos la dirección del hostname:port */
  if (getaddrinfo(ip, puerto, NULL, &resultado)){
    fprintf(stderr,"No se encontro el host: %s \n",ip);
    cerrar_cliente();
  }

  estado++; 
  if(connect(socksrv, (struct sockaddr *) resultado->ai_addr, resultado->ai_addrlen) != 0){
    cerrar_cliente();
    error("No se pudo conectar con el server :(. ");
  }

  return socksrv;
}

void upload_data(char* argv[]){

  strncpy(ip,argv[1],sizeof(ip)+1);
  strncpy(puerto,argv[2],sizeof(puerto));

}

void hilos(){


  pthread_create(&clienteEscribir , NULL , escribir, (void*) &sock);
  pthread_create(&clienteRecibir , NULL , recibir,(void*) &sock);
  
  pthread_join(clienteRecibir,NULL);
  pthread_join(clienteEscribir,NULL);

}
