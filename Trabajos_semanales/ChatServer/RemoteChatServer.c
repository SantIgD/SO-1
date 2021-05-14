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

#define DISPONIBLE -1
#define DESCONECTAR 0
#define PRIVADO 1
#define NUEVONICK 2
#define ALL 3

#define NICKNAME_EN_USO "El nickname ya esta en uso, vuelva a ingresar"

/* Tamaño del buffer */
#define BSIZE 1024

/* Tamaño maximo de Nickname*/
#define NNSIZE 100

char mensaje[BSIZE];
int clientes[MAX_CLIENTS];
char nicknames[MAX_CLIENTS][BSIZE];
int clientesConectados = 0;
pthread_mutex_t candado;


/* Encargado de manejarse con un cliente */
void *moderador(void *arg);

/* Definimos una pequeña función auxiliar de error */
void error(char *msg);

/* Si el nickname existe, devuelve el indice al que corresponde. Sino -1*/
int indice_nickname(char nickname[BSIZE]);

/* Verifica la operacion que se recibe desde un cliente*/
int verificar_operacion(char buf[BSIZE]);

/* Crea y conecta un socket a una direccion */
int crear_inicializar_socket(int sock, struct sockaddr_in servidor, int puerto);

/* Retorna -1 en el caso de que el nickname este libre*/
int is_nickname_disponible(char* nickname);

/* Ejecuta la operacion recibida */
int ejecutar_operacion(int indiceSockClient, char* buff, int operacion);

/* Interpreta y acciona apartir del mensaje recibido*/
void procesar_mensaje(int indiceSockClient,char* buff);

int main(int argc, char **argv){

  int sock, *soclient;
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;
  int argumentos[MAX_CLIENTS];
  
  if (argc != 2) error("Faltan argumentos");

  pthread_mutex_init(&candado,NULL);

  sock = crear_inicializar_socket(sock,servidor,htons(atoi(argv[1])));
 
  printf("Enlazamiento exitoso, escuchando a %s\n",argv[1]);

  /********************/
  /* Creamos los atributos para los hilos.*/
  pthread_attr_init(&attr);
  /* Hilos que no van a ser joinables */
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);
  /********************/

  /* Ya podemos aceptar conexiones */
  if(listen(sock, MAX_CLIENTS) == -1)
    error(" Listen error ");
  
  
  for(;;){ 

    /* Pedimos memoria para el socket */
    soclient = malloc(sizeof(int));

    /* Cuando llegue un nuevo cliente, le vamos a aceptar*/
    clientelen = sizeof(clientedir);
    if ((*soclient = accept(sock
                          , (struct sockaddr *) &clientedir
                          , &clientelen)) == -1)
      error("No se puedo aceptar la conexión. ");
    
    clientes[clientesConectados] = *soclient;
    argumentos[clientesConectados] = clientesConectados;
    
    /* Le enviamos el socket al hijo*/
    pthread_create(&thread , &attr , moderador, (void *) &argumentos[clientesConectados]);

    

    /* El servidor puede hacer alguna tarea más o simplemente volver a esperar*/
  }

  /* Código muerto */
  close(sock);

  return 0;
}

int obtener_nickname(char nicknamePrivado[BSIZE],char buf[BSIZE]){
    int i,cont=0;
    for(;buf[i]!=' ' && buf[i]!='\n';i++);

    i++;

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
        int indice=indice_nickname(nicknamePrivado);
        int sock=clientes[indice];
        //char mensaje[BSIZE]=obtener_mensaje(buf,);
    }
    else{
        /*Hagop algo*/
        
    }
    
}

void cambiar_nickname(int sock,char buf[BSIZE]){
    
    
    
}

void send_all(char buf[BSIZE]){
    
    for(int i=0;i<clientesConectados;i++)
      send(clientes[i],buf,sizeof(buf), 0);

}

void mensaje_all(char buf[BSIZE],int indiceSockClient){
  char buffer[BSIZE];
  
 
  strcpy(buffer,nicknames[indice_nickname]);
  strncat(buffer," >> ",sizeof(" >> "));
  strcat(buffer,buf);
    
  pthread_mutex_lock(&candado);  
  send_all(buffer)
  pthread_mutex_unlock(&candado);  
  
}


int ejecutar_operacion(int indiceSockClient, char* buff, int operacion){
    

    switch(operacion){

        case DESCONECTAR:
        {
            desconectar_cliente(indiceSockClient);
            break;
        }

        case PRIVADO:
        {
            mensaje_privado(buf);
            break;
        }

        case ALL:
        {
            mensaje_all(buf,indiceSockClient);
            break;
        }
        case NUEVONICK
        {
        
            break;
        }
    }
    
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

void procesar_mensaje(int indiceSockClient,char* buff){

    int op = verificar_operacion(buff);

    ejecutar_operacion(indiceSockClient,buf,op);
    
}

/*
*/
void * moderador(void *_arg){
  int indiceSockClient = *(int*) _arg;
  char buf[BSIZE];
  char nickname[BSIZE];
  char auxiliar[BSIZE];
  int sock = clientes[indiceSockClient];

  
  /*Pedimos un nickname al cliente*/
  send(sock , "Ingrese su nickname",sizeof("Ingrese su nickname"), 0);

  /* Recibimos Nombre de usuario */
  recv(sock, nickname, sizeof(nickname), 0);
  
  /*Primera verificacion de nickname*/
  pthread_mutex_lock(&candado);  
  
  /*Verificamos que el cliente tenga un nickname posible*/
  while (is_nickname_disponible(nickname) != DISPONIBLE){
        
        pthread_mutex_unlock(&candado);      
        
        send(sock ,NICKNAME_EN_USO , sizeof(NICKNAME_EN_USO), 0);
      
        recv(sock, nickname, sizeof(nickname), 0);
        
        procesar_mensaje(sock,nickname);

        pthread_mutex_lock(&candado);
        
  }    

  clientesConectados++;

  strcpy(nicknames[indiceSockClient],nickname);
  pthread_mutex_unlock(&candado);    
  
  strcpy(buf,"Tu nickname en el servidor es : ");
  strcat(buf,nickname);
  send(sock , buf,sizeof(buf), 0);



  /*Ajustamos el propmt del nickname*/
  /*
  strncat(nickname," >> ",sizeof(" >> "));

  strcpy(auxiliar,"");

  printf("[Nickname] %s\n",nickname);
*/
  
  while(1){

    /* Esperamos mensaje */
    recv(sock, buf, sizeof(buf), 0);

    procesar_mensaje(indiceSockClient,buf);
    
  }
  

  free((int*)_arg);
  return NULL;
}

int is_nickname_disponible(char* nickname){

    return indice_nickname(nickname);
}

void error(char *msg){
  exit((perror(msg), 1));
}


int indice_nickname(char nickname[BSIZE]){
    int longitud = MAX_CLIENTS;//cambiar por cantCli8entes??????
    int rest = DISPONIBLE;
    for(int i = 0;i<longitud && rest==-1 ;i++){
        if(strcmp(nicknames[i],nickname)== 0)
            rest=i;
    }
    
    return rest;
}

int verificar_operacion(char buf[BSIZE]){
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
