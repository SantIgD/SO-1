/* RemoteMultiThreadServer.c */
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netinet/in.h>
/**********/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
/**********/
/* Threads! */
#include <pthread.h>

/* Asumimos que el primer argumento es el puerto por el cual escuchará nuestro
servidor */

/* Maxima cantidad de cliente que soportará nuestro servidor */
#define MAX_CLIENTS 25

char mensaje[1024];
int clientes[MAX_CLIENTS];
int clientesConectados = 0;
pthread_mutex_t candado;

/**/

/* Anunciamos el prototipo del hijo */
void *child(void *arg);
/* Definimos una pequeña función auxiliar de error */
void error(char *msg);

int main(int argc, char **argv){
  int sock, *soclient;
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;
  int argumentos[MAX_CLIENTS];
  pthread_mutex_init(&candado,NULL);

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

  /************************************************************/
  /* Creamos los atributos para los hilos.*/
  pthread_attr_init(&attr);
  /* Hilos que no van a ser *joinables* */
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  /************************************************************/

  /* Ya podemos aceptar conexiones */
  if(listen(sock, MAX_CLIENTS) == -1)
    error(" Listen error ");
  
  
  for(;;){ /* Comenzamos con el bucle infinito*/
    /* Pedimos memoria para el socket */
    soclient = malloc(sizeof(int));

    /* CANDADO */

    /* Now we can accept connections as they come*/
    clientelen = sizeof(clientedir);
    if ((*soclient = accept(sock
                          , (struct sockaddr *) &clientedir
                          , &clientelen)) == -1)
      error("No se puedo aceptar la conexión. ");
    
    clientes[clientesConectados]=*soclient;
    argumentos[clientesConectados] = clientesConectados;
    
    /* Le enviamos el socket al hijo*/
    pthread_create(&thread , NULL , child, (void *) &argumentos[clientesConectados]);

    clientesConectados++;

    /* El servidor puede hacer alguna tarea más o simplemente volver a esperar*/
  }

  /* Código muerto */
  close(sock);

  return 0;
}


/*
*/
void * child(void *_arg){
  int indiceSockClient = *(int*) _arg;
  char buf[1024];
  char nickname[1024];
  char auxiliar[1024];
  int sock = clientes[indiceSockClient];


  /* Pedimos nombre de usuario */
  send(sock, "Ingrese Nickname", sizeof("Ingrese Nickname"), 0);
  /* Recibimos Nombre de usuario */
  recv(sock, buf, sizeof(buf), 0);

  


  strncpy(nickname,buf,sizeof(nickname));

  ///strncat("[",nickname,sizeof(nickname));
  strncat(nickname," >> ",sizeof(" >> "));

  //int lenNicknickname=sizeof(nickname)
  strcpy(auxiliar,"");

  printf("[Nickname] %s\n",nickname);

  while(1){

    /* Esperamos mensaje */
    recv(sock, buf, sizeof(buf), 0);

    printf("[Mensaje] >%s<\n",buf);
    strncat(auxiliar,nickname,sizeof(nickname));
    strncat(auxiliar,buf,sizeof(buf));

    printf("[Mensaje] >%s<\n",auxiliar);

    pthread_mutex_lock(&candado);
    for(int i = 0; i < clientesConectados; i++){
      
      send(clientes[i],auxiliar,sizeof(auxiliar), 0);
      
    }
    pthread_mutex_unlock(&candado);

    strcpy(auxiliar,"");
  }
  

  /*Chequear que el nombre sea valido*/

  //while (!nickNameValido)

    /*Candado*/

    //nickNameValido = Validar nombre. -> Region Critica

    /*Candado*/

    // If no valido -> El nickname ingresado ya esta ocupado, porfavor ingrese otro.

  //endWhile

// A la variable GLobal con Nicks, le agregas el nick de este cliente.

//  [NICKNAME] mensaje


/* loopInfinito 

  Espero mensaje, ESTOY BLOQUEADO

  Recibo mensaje -> Candado





*/







  free((int*)_arg);
  return NULL;
}

void error(char *msg){
  exit((perror(msg), 1));
}
