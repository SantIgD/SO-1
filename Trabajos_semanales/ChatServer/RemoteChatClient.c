/* RemoteClient.c
   Se introducen las primitivas necesarias para establecer una conexión simple
   dentro del lenguaje C utilizando sockets.
*/
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netdb.h>
/****/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <signal.h>


#define MAXSIZEMSG 1024
char ip[100];
char puerto[100];
int sock;
struct addrinfo *resultado;
int estado=-1;
int bandera=0;

/*
  El archivo describe un sencillo cliente que se conecta al servidor establecido
  en el archivo RemoteServer.c. Se utiliza de la siguiente manera:
  $cliente IP port
 */


/*Printea error con el mensaje msg por pantalla*/
void error(char *msg);

/* Desconexion y cierre del cliente */
void cerrar_cliente();

/* Manejo de signals */
void handler(int arg);

/* Esto no deberia estar aca o no lo entiendo */
int verificar_opcion(char buf[MAXSIZEMSG]);

/* Inicializa y conecta un socket a un ip:puerto*/
int conectar_socket_red();

/* Hilo para mandar mensajes al servidor */
void* escribir(void * arg);

/* Hilo para recibir mensajes del servidor */
void* recibir (void * arg);

void upload_data(char* argv[]){

  strncpy(ip,argv[1],sizeof(ip)+1);
  strncpy(puerto,argv[2],sizeof(puerto));

}

int main(int argc, char **argv){

  pthread_t clienteEscribir, clienteRecibir;

  //Chequeamos mínimamente que los argumentos fueron pasados/
  
  if(argc != 3){
    fprintf(stderr,"La cantidad de argumentos no es correcta.\n");  
    fprintf(stderr,"El uso es \'%s IP port\'", argv[0]);
    exit(1);
  }
  
  signal(SIGINT, handler);
  signal(SIGTERM, handler);
  
  //Cargamos los datos necesarios/
  upload_data(argv);
    
  //Conectamos con el server/    
  sock = conectar_socket_red();

  printf("Se ha podido establecer conexion con el server %s:%s\n",ip,puerto);
  
  pthread_create(&clienteEscribir , NULL , escribir, (void*) &sock);
  pthread_create(&clienteRecibir , NULL , recibir,(void*) &sock);

 //Empezamos la comunicacion en el chat/
  /* bucle infinito*/
  
  
  pthread_join(clienteRecibir,NULL);
  pthread_join(clienteEscribir,NULL);

   /* Cerramos :D!*/
  freeaddrinfo(resultado);
  close(sock);

  return 0;
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
    cerrar_cliente();
}

int verificar_opcion(char buf[MAXSIZEMSG]){
    char aux[15]="";
    int ret=-1;
    if(buf[0]=='/'){
        for(int i = 0;buf[i]!=' ' && i < 15;i++){
            aux[i]=buf[i];
        }
        if(strcmp(aux,"/exit")==0){
            ret=0;
        }
        else if(strcmp(aux,"/msg")==0){
            ret=1;
        }
        else if(strcmp(aux,"/nickname")==0){
            ret=2;
        }
    }    
    return ret;    
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
  if(connect(socksrv, (struct sockaddr *) resultado->ai_addr, resultado->ai_addrlen) != 0)
    /* if(connect(sock, (struct sockaddr *) &servidor, sizeof(servidor)) != 0) */
    cerrar_cliente();
    error("No se pudo conectar con el server :(. ");

  return socksrv;
}

void* escribir(void * arg){

  int sock = *(int *) arg;
  char buf[MAXSIZEMSG];
  ssize_t bytes;

  while(1){

    scanf(" %[^\n]s",buf);
    send(sock,buf,sizeof(buf),0);
    /*Verificamos si el cliente se quiere desconectar del server*/
    if(verificar_opcion(buf)==0){
        bandera++;
    }

  }

}

void* recibir (void * arg){

  int sock = *(int *) arg;
  char buf[MAXSIZEMSG];
  ssize_t bytes;


  while(1){

    bytes = recv(sock, buf, sizeof(buf),0);

    buf[bytes] = '\0';
    
    printf("%s\n", buf);
  }

}
