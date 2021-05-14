/* RemoteMultiThreadServer.c */
/* Cabeceras de Sockets */
#include <sys/types.h>
#include <sys/socket.h>
/* Cabecera de direcciones por red */
#include <netinet/in.h>
/****/
#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
/****/
/* Threads! */
#include <pthread.h>

/* Asumimos que el primer argumento es el puerto por el cual escuchará nuestro
servidor */

/* Maxima cantidad de cliente que soportará nuestro servidor */
#define MAX_CLIENTS 25

/* Tamaño del buffer */
#define BSIZE 1024

/* Tamaño maximo de Nickname*/
#define NNSIZE 100

char mensaje[BSIZE];
int clientes[MAX_CLIENTS];
char nicknames[MAX_CLIENTS][128];
int clientesConectados = 0;
pthread_mutex_t candado;

/**/



/* Anunciamos el prototipo del hijo */
void *child(void *arg);
/* Definimos una pequeña función auxiliar de error */
void error(char *msg);

int validar_nickname(char nickname[BSIZE]){
    int longitud = MAX_CLIENTS;
    int rest=-1;
    for(int i = 0;i<longitud && rest==-1 ;i++){
        if(strcmp(nicknames[i],nickname)== 0)
            rest=i;
    }
    
    return rest;
}

int verificar_opcion(char buf[BSIZE]){
    char aux[15]="";
    int ret=3;
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

int crear_inicializar_socket(int sock, struct sockaddr_in servidor, int puerto){

   /* Creamos el socket */
  if( (sock = socket(AF_INET, SOCK_STREAM, 0)) < 0 )
    error("Socket Init");

  /* Creamos a la dirección del servidor.*/
  servidor.sin_family = AF_INET; /* Internet */
  servidor.sin_addr.s_addr = INADDR_ANY; /**/
  servidor.sin_port = puerto;

  /* Inicializamos el socket */
  if (bind(sock, (struct sockaddr *) &servidor, sizeof(servidor)))
    error("Error en el bind");

  return sock;
}

int main(int argc, char **argv){
  int sock, *soclient;
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;
  int argumentos[MAX_CLIENTS];
  pthread_mutex_init(&candado,NULL);

  if (argc <= 1) error("Faltan argumentos");

  sock = crear_inicializar_socket(sock,servidor,htons(atoi(argv[1])));
 
  printf("Binding successful, and listening on %s\n",argv[1]);

  /********************/
  /* Creamos los atributos para los hilos.*/
  pthread_attr_init(&attr);
  /* Hilos que no van a ser joinables */
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  /********************/

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

int obtener_nickname(char nicknamePrivado[BSIZE],char buf[BSIZE],int opcion){
    int i,cont=0;

    if (opcion==1)
        i=5;
    else
        i=10;
    for(;buf[i]!=' ' && buf[i]!='\n' ;i++){
        nicknamePrivado[i]=buf[i];
        cont++;
    }
    return cont;
}


void mensaje_privado(char buf[BSIZE]){
    /*Hacer control de errores de mnsages y nicknames*/
    char nicknamePrivado[BSIZE]="";
    int lenNickname=obtener_nickname(nicknamePrivado,buf,1);
    if(lenNickname>0){
        int indice=validar_nickname(nicknamePrivado);
        int sock=clientes[indice];
        //char mensaje[BSIZE]=obtener_mensaje(buf,);
    }
    else{
        /*Hagop algo*/
        
    }
    
}

void cambiar_nickname(int sock,char buf[BSIZE]){
    
    
    
}

int establer_opcion_comunicacion(int sock,char buf[BSIZE]){
    int ret=verificar_opcion(buf);
    
    if(ret==1){
        mensaje_privado(buf);
    }
    else if(ret==2){
        cambiar_nickname(sock,buf);   
    }
    else if(ret==3){
        send(sock ,"Comando no valido",sizeof("Comando no valido"), 0);
    }
    
    return ret;
}


/*
*/
void * child(void *_arg){
  int indiceSockClient = *(int*) _arg;
  char buf[BSIZE];
  char nickname[BSIZE];
  char auxiliar[BSIZE];
  int sock = clientes[indiceSockClient];
  int bandera=1;



  /* Recibimos Nombre de usuario */
  recv(sock, buf, sizeof(buf), 0);
    
  strncpy(nickname,buf,sizeof(nickname));    
  /*Primera verificacion de nickname*/
  pthread_mutex_lock(&candado);  
  
  /*Verificamos que el cliente tenga un nickname posible*/
  while (validar_nickname(nickname)==-1){
        
        pthread_mutex_unlock(&candado);      
            
        strcpy(buf,"En nickname ya esta en uso vuelva a intentar");
        
        send(sock , buf,sizeof(buf), 0);
        
        recv(sock, buf, sizeof(buf), 0);
        
        strncpy(nickname,buf,sizeof(nickname));  
        
        pthread_mutex_lock(&candado);
        
  }    
  pthread_mutex_unlock(&candado);    
  
  strcpy(nicknames[indiceSockClient],nickname);
  
  /*Ajustamos el propmt del nickname*/
  /*
  strncat(nickname," >> ",sizeof(" >> "));

  strcpy(auxiliar,"");

  printf("[Nickname] %s\n",nickname);
*/
  while(bandera){

    /* Esperamos mensaje */
    recv(sock, buf, sizeof(buf), 0);

    bandera=establer_opcion_comunicacion(sock,buf);
    
    /*
    strncat(auxiliar,nickname,sizeof(nickname));
    strncat(auxiliar,buf,sizeof(buf));


    pthread_mutex_lock(&candado);
    for(int i = 0; i < clientesConectados; i++){
      
      send(clientes[i],auxiliar,sizeof(auxiliar), 0);
      
    }
    pthread_mutex_unlock(&candado);


    strcpy(auxiliar,"");
    */
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
