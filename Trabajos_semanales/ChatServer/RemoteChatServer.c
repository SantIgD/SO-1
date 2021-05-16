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

#define CLIENTE_DESCONECTAR 10
#define MENSAJE_ALL 11
#define MENSAJE_PRIVADO 12
#define CLIENTE_NUEVO_NICK 13





#define EXITMSG "OK"
#define NICKNAME_EN_USO "[ChatServer] El nickname ingresado ya esta en uso, porfavor vuelva a ingresar otro"
#define NICKNAME_OTORGADO "[ChatServer] Su nickname en el Chat es: "
#define NICKNAME_REQUEST "[ChatServer] Ingrese su nickname"
#define NICKNAME_NUEVO_OTORGADO "[ChatServer] Su nuevo nickname en el Chat es: "


#define ERROR_MENSAJE_PRIVADO1 "[ChatServer] El nickname ingresado no coincide con ningun usuario conectado"
#define ERROR_MENSAJE_PRIVADO2 "[ChatServer] Falta el destinatario"
#define ERROR_MENSAJE_PRIVADO3 "[ChatServer] Falta el mensaje" 

#define ERROR_CAMBIO_NICKNAME "[ChatServer] Falta ingresar el nuevo nickname"

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
int ejecutar_operacion(int sock,char buf[BSIZE], int operacion,char nickname[BSIZE]);

/* Interpreta y acciona apartir del mensaje recibido*/
int procesar_mensaje(int sock,char buff[BSIZE],char nickname[BSIZE]);

int verificar_operacion(char buf[BSIZE]);

void imprimir_nicknames();

/* Obtiene el nickname del mensaje recibido por el cliente, tanto en nuevo nick como en privado*/
int obtener_nickname(char nicknamePrivado[BSIZE],char buf[BSIZE]);

/* Obtiene el mensaje a enviar por privado*/
int obtener_mensaje(char buffer[BSIZE],char buf[BSIZE]);

int main(int argc, char **argv){

  int sock, *soclient;
  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;
  //int argumentos[MAX_CLIENTS];
  
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
    //argumentos[clientesConectados] = clientesConectados;
    
    /* Le enviamos el socket al hijo*/
    pthread_create(&thread , &attr , moderador, (void *) soclient);

    

    /* El servidor puede hacer alguna tarea más o simplemente volver a esperar*/
  }

  /* Código muerto */
  close(sock);

  return 0;
}

int obtener_nickname(char nicknamePrivado[BSIZE],char buf[BSIZE]){
    
    char* token = strtok(buf, " "); // en token queda la operacion
    int cont = 0,ret = 0;

    while (token != NULL && cont != 2){

        cont++;
        
        
        if(cont==2){
            strcpy(nicknamePrivado,token);
        }

        token = strtok(NULL, " ");

    }

    if (cont < 2){
        ret = -1; // No habia nickname
    }

    
    return ret;
    
}

int obtener_mensaje(char buffer[BSIZE],char buf[BSIZE]){
    

        printf("[Buffer] %s\n",buf);
    char* token = strtok(buf, " "); // en token queda la operacion
    int cont = 0,ret = 0;


        printf("[Token] %s\n",token);

    while (token != NULL){

        cont++;

        if(cont > 2 && token != NULL){
            strncat(buffer,token,sizeof(token));
            strncat(buffer," ",sizeof(token));
        }

        token = strtok(NULL, " ");
        
    }

    if (cont < 2){
        ret = -1; // No habia mensaje
    }

    printf("[Mensaje] %s\n",buffer);

    
    return ret;
}

int mensaje_privado(int sock,char buf[BSIZE],char nickname[BSIZE]){
    //Hacer control de errores de mnsages y nicknames
    char nicknamePrivado[BSIZE]="";

    int lenNickname;
    int indice;
    char msg[BSIZE]="";
    char aux[BSIZE]="";
    char aux2[BSIZE]="";

    strcpy(aux2,buf);
     

    //printf("[Buffer] %s\n",buf);

    if (obtener_nickname (nicknamePrivado,aux2) == -1){

        send(sock,ERROR_MENSAJE_PRIVADO2,sizeof(ERROR_MENSAJE_PRIVADO2), 0);

        return -1;
    }

    //printf("[Buffer] %s\n",buf);

    if (obtener_mensaje(aux,buf) == -1){

        send(sock,ERROR_MENSAJE_PRIVADO3,sizeof(ERROR_MENSAJE_PRIVADO3), 0);
        return -1;
    }

    

    strcpy(msg,nickname);
    strncat(msg," >> ",sizeof(" >> "));

    strncat(msg,aux,sizeof(aux));
  
    //printf("[msg] %s\n",aux);

    
    int sockcli;
    

    //obtener_nickname(nicknamePrivado,buf);

    if (lenNickname = strlen(nicknamePrivado)){

        indice = indice_nickname(nicknamePrivado);
        
        //printf("indice %d",indice);
        
        if(indice != -1){

            sockcli = clientes[indice];
            send(sockcli,msg,strlen(msg), 0);

        }else{

            send(sock,ERROR_MENSAJE_PRIVADO1,sizeof(ERROR_MENSAJE_PRIVADO1), 0);
        }

    }else{

        send(sock,ERROR_MENSAJE_PRIVADO2,sizeof(ERROR_MENSAJE_PRIVADO2), 0);
    }

}

int cambiar_nickname(int sock,char buf[BSIZE],char nickname[BSIZE]){
    
    char nuevoNickName[BSIZE];
    char msg[BSIZE];

    int lenNickname,ret=-1;

    if (obtener_nickname(nuevoNickName,buf) != -1){

        lenNickname = sizeof(nuevoNickName);
        int indice; 

        if(lenNickname > 0){

            if (is_nickname_disponible(nuevoNickName) == DISPONIBLE ){

                indice = indice_nickname(nickname);

                strcpy(nicknames[indice],nuevoNickName);
                strcpy(nickname,nuevoNickName);
                
                
                strcat(msg,NICKNAME_NUEVO_OTORGADO);
                strcat(msg,nuevoNickName);
                send(sock,msg,sizeof(msg), 0);

                ret = 0;
            }
            else{
                send(sock,NICKNAME_EN_USO,sizeof(NICKNAME_EN_USO), 0);
            }
        }
        else{
            send(sock,ERROR_CAMBIO_NICKNAME,sizeof(ERROR_CAMBIO_NICKNAME), 0);
        }
    }

    return ret;

}

void send_all(char buf[BSIZE]){
    
    for(int i=0;i<clientesConectados;i++)
      send(clientes[i],buf,strlen(buf), 0);

}

void mensaje_all(char buf[BSIZE],char nickname[BSIZE]){
  char buffer[BSIZE];
  
  
  strcpy(buffer,nickname);
  strncat(buffer," >> ",sizeof(" >> "));
  strcat(buffer,buf);

  send_all(buffer);

}

void clientes_fix_array(char nickname[BSIZE]){

    int indice = indice_nickname(nickname);
    clientesConectados--;
    int indiceUltimo = clientesConectados;

    printf("clientes[%d] %s se desconecta,\nclientes[%d] %s reemplazo su lugar\n",indice,nicknames[indice],indiceUltimo,nicknames[indiceUltimo]);
    

    if (indice != indiceUltimo){

        strcpy(nicknames[indice],nicknames[indiceUltimo]);
        clientes[indice] = clientes[indiceUltimo];
    }

}

int desconectar_cliente(int sock,char nickname[BSIZE]){

    clientes_fix_array(nickname);
    //imprimir_nicknames();
    send(sock,EXITMSG,sizeof(EXITMSG), 0);
    return 0;

}

int ejecutar_operacion(int sock,char buf[BSIZE], int operacion,char nickname[BSIZE]){
    int salir = 1;

    switch(operacion){

        case CLIENTE_DESCONECTAR:
        {
            salir = desconectar_cliente(sock,nickname);
            break;
        }

        case MENSAJE_PRIVADO:
        {
            mensaje_privado(sock,buf,nickname);
            break;
        }

        case MENSAJE_ALL:
        {
            mensaje_all(buf,nickname);
            break;
        }
        case CLIENTE_NUEVO_NICK:
        {
            cambiar_nickname(sock,buf,nickname);
            break;
        }
    }
    return salir;
}

int procesar_mensaje(int sock,char buff[BSIZE],char nickname[BSIZE]){

    int op = verificar_operacion(buff);
    
    
    pthread_mutex_lock(&candado);
    op=ejecutar_operacion(sock,buff,op,nickname);
    pthread_mutex_unlock(&candado);
    return op;
}

int is_nickname_disponible(char* nickname){

    return indice_nickname(nickname);
}

void error(char *msg){
  exit((perror(msg), 1));
}

int indice_nickname(char nickname[BSIZE]){
    int longitud = clientesConectados;
    int ret = DISPONIBLE;

    for(int i = 0; i < longitud && ret == -1 ;i++){
        //printf("|%s|%s|%d\n",nicknames[i],nickname,strcmp(nicknames[i],nickname));
        if(strcmp(nicknames[i],nickname) == 0){
            ret=i;
            //printf("Se encontro un nombre igual\n");
        }
            
    }
    
    return ret;
}

int verificar_operacion(char buf[BSIZE]){
    char aux[15]="";
    int ret = MENSAJE_ALL;

    if(buf[0] == '/'){

        for(int i = 0;buf[i]!=' ' && i < 15;i++){
            aux[i]=buf[i];
        }

        if(strcmp(aux,"/exit")==0){
            ret = CLIENTE_DESCONECTAR;
        }
        else if(strcmp(aux,"/msg")==0){
            ret = MENSAJE_PRIVADO;
        }
        else if(strcmp(aux,"/nickname")==0){
            ret = CLIENTE_NUEVO_NICK;
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

void imprimir_nicknames(){
    
    printf("[Nicknames]\n");
    for(int i = 0; i < clientesConectados; i++)
        printf("[Cliente:Nickname] [%d:%s]\n",clientes[i],nicknames[i]);
    printf("______________________________\n");

}

void * moderador(void *_arg){
  int sock = *(int*) _arg;
  char buf[BSIZE];
  char nickname[BSIZE];
  char auxiliar[BSIZE];
  int clienteOn = 1;
      
  /*Pedimos un nickname al cliente*/
  send(sock , NICKNAME_REQUEST,sizeof(NICKNAME_REQUEST), 0);

  /* Recibimos Nombre de usuario */
  recv(sock, nickname, sizeof(nickname), 0);
  
  /*Primera verificacion de nickname*/
  pthread_mutex_lock(&candado);  
  
  /*Verificamos que el cliente tenga un nickname posible*/
  while (is_nickname_disponible(nickname) != DISPONIBLE){
        
        pthread_mutex_unlock(&candado);      
        
        send(sock ,NICKNAME_EN_USO , sizeof(NICKNAME_EN_USO), 0);
      
        recv(sock, nickname, sizeof(nickname), 0);

        pthread_mutex_lock(&candado);
  }    

  
  strcpy(nicknames[clientesConectados],nickname);
  clientesConectados++;

  imprimir_nicknames();

  pthread_mutex_unlock(&candado);    
  
  strcpy(buf,NICKNAME_OTORGADO);
  strcat(buf,nickname);
  send(sock , buf,sizeof(buf), 0);

  while(clienteOn){

    /* Esperamos mensaje */
    recv(sock, buf, sizeof(buf), 0);

    
    clienteOn = procesar_mensaje(sock,buf,nickname);
  }
  

  free((int*)_arg);
  return NULL;
}
