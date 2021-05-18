
/*****************************/

/* RemoteMultiThreadServer.c */

/*****************************/



/* Cabeceras de Sockets */
/*****************************/

#include <sys/types.h>
#include <sys/socket.h>

/*****************************/


/* Cabecera de direcciones por red */
/*****************************/

#include <netinet/in.h>

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

/*****Macros*****/
/**************************************************************/

/* Maxima cantidad de cliente que soportará nuestro servidor */
#define MAX_CLIENTS 25

/*Disponibilidad*/
#define DISPONIBLE -1
#define NO_DISPONIBLE 1

/*Comandos del chat*/
#define CLIENTE_DESCONECTAR 10
#define MENSAJE_ALL 11
#define MENSAJE_PRIVADO 12
#define CLIENTE_NUEVO_NICK 13

/*salida*/
#define EXIT 0

#define NOTIFICADO 10

/*Longitud maxima de los nicknames*/
#define MAX_NICKNAME_SIZE 20

/*Nombre por default*/
#define DEFAULTNICK "noname"

/*Macros para cerrar el server y sacar a un cliente*/
#define SHUTDOWN "shutdown"
#define EXITMSG "OK"

/*Mensajes del servidor para los clientes*/
#define NICKNAME_EN_USO "[ChatServer] El nickname ingresado ya esta en uso, porfavor vuelva a ingresar otro"
#define NICKNAME_OTORGADO "[ChatServer] Su nickname en el Chat es: "
#define NICKNAME_REQUEST "[ChatServer] Ingrese su nickname"
#define NICKNAME_NUEVO_OTORGADO1 "[ChatServer] Su nuevo nickname en el Chat es: "
#define NICKNAME_NUEVO_OTORGADO2 " a cambiado su nickname a: "
#define NICKNAME_NOTIFICATION_ALL "[ChatServer] Bienvenido a la sala "

/*Mensajes de error del servidor para los clientes*/
#define ERROR_MENSAJE_PRIVADO1 "[ChatServer] El nickname ingresado no coincide con ningun usuario conectado"
#define ERROR_MENSAJE_PRIVADO2 "[ChatServer] Falta el destinatario"
#define ERROR_MENSAJE_PRIVADO3 "[ChatServer] Falta el mensaje" 
#define ERROR_CAMBIO_NICKNAME "[ChatServer] Falta ingresar el nuevo nickname"


/* Tamaño del buffer */
#define BSIZE 1024

/* Tamaño maximo de Nickname*/
#define NNSIZE 100

/**************************************************************/

/*Varibles globales*/
/**************************************************************/

char mensaje[BSIZE];//¡¡¡¡¡¡¡¡¡sacar¡¡¡¡¡
int clientes[MAX_CLIENTS];//Arreglo donde almacenamos los sockets de los clientes
char nicknames[MAX_CLIENTS][BSIZE];//Arreglo donde se almacenan los nicknames de los clientes
int clientesConectados = 0;//Contador de clientes conectados
pthread_mutex_t candado;//Candado utilizado para evitar la superposicion de operaciones del server
int socksrv;//Socket principal del servidor
int** dirClientes;//Arreglo donde estan almacenadas las direcciones de memoria de los sockets de los clientes
int *soclient;

/**************************************************************/


/*Funciones del servidor*/
/**************************************************************/

/* Recibe interrupciones y termina elegantemente*/
void handler(int arg);

/* Encargado de manejar con un cliente */
void *moderador(void *arg);

/* Busca el indice del cliente asociado al socket*/
int obtener_indice_cliente(int sock);

/* Pide el nombre de usuario por primera vez*/
int ask_nickname_1(int sock,int indice,char nickname[BSIZE]);

/* Pide el nombre de usuario hasta que se ingrese correctamente*/
int ask_nickname_2(int sock,int indice,char nickname[BSIZE]);

/* Verifica si el cliente quiere salir*/
int check_exit_msg(char nickname[BSIZE],int indice,int sock);

/* Obtiene el nickname del mensaje recibido por el cliente, tanto en cambiar_nickname como en mensaje_privado*/
int obtener_nickname(char nicknamePrivado[BSIZE],char buf[BSIZE]);

/* Obtiene el mensaje a enviar por privado*/
int obtener_mensaje(char buffer[BSIZE],char buf[BSIZE]);

/*Prepara y envia un mensaje privado a un cliente*/
int mensaje_privado(int sock,char buf[BSIZE],char nickname[BSIZE]);

/* Envia el mensaje a todos los clientes conectados*/
void send_all(char buf[BSIZE]);

/*Cambia el nickname asociado al sock por uno nuevo*/
int cambiar_nickname(int sock,char buf[BSIZE],char nickname[BSIZE]);



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














/* Desconecta el cliente */
int desconectar_cliente(int sock,char nickname[BSIZE]);

/**************************************************************/



/* Preparar notificacion de nuevo nickname a todos los users*/
void create_msg_nuevo_nickname_notification(int indice, char nickname[BSIZE], char buf[BSIZE]);

/* Funcion encargada de resolver cuestiones de la asignacion del nickname*/
int nickname_request_manager(int sock, int indice, char nickname[BSIZE],char buf[BSIZE], int nickNameFlag);


/* Recibe interrupciones y termina elegantemente*/
void handler(int arg){

  char buf[BSIZE];
  strcpy(buf,EXITMSG);
  send_all(buf);
  
  for(int i=0;i<clientesConectados;i++){
    close(clientes[i]);
    free(dirClientes[i]);
  }
  
  free(soclient);
  free(dirClientes);
  exit(EXIT_SUCCESS);  
}


/* Asumimos que el primer argumento es el puerto por el cual escuchará nuestro
servidor */
/* Funcion princcipal del servidor*/

int main(int argc, char **argv){

  struct sockaddr_in servidor, clientedir;
  socklen_t clientelen;
  pthread_t thread;
  pthread_attr_t attr;

  /*Peparamos el manejo de señales*/
  signal(SIGINT, handler);
  signal(SIGTERM, handler);

  /*Verificamos que la cnatidad de argumentos recividos se la correcta*/
  if (argc != 2) error("Faltan dirClientes");

  /*Inicializamos el candado*/  
  pthread_mutex_init(&candado,NULL);

  /*Creamos un socket para el server*/
  socksrv = crear_inicializar_socket(socksrv,servidor,htons(atoi(argv[1])));
 
  printf("Enlazamiento exitoso, escuchando a %s\n",argv[1]);

  /********************/
  
  /* Creamos los atributos para los hilos.*/
  pthread_attr_init(&attr);
  /* Hilos que no van a ser joinables */
  pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_DETACHED);

  /********************/

  /* Ya podemos aceptar conexiones */
  if(listen(socksrv, MAX_CLIENTS) == -1)
    error(" Listen error ");
  
  /*Asignamos memoria al arrglo de dirClientes*/
  dirClientes=malloc(sizeof(int)* MAX_CLIENTS);

  while(1){ 

    /* Pedimos memoria para el socket */
    soclient = malloc(sizeof(int));

    /* Cuando llegue un nuevo cliente, le vamos a aceptar*/
    clientelen = sizeof(clientedir);
    if ((*soclient = accept(socksrv
                          , (struct sockaddr *) &clientedir
                          , &clientelen)) == -1)
      error("No se puedo aceptar la conexión. ");

    
    /*Colocamos al socket del cliente recibido en un arreglo y le
      asignamos un nombre probicional*/    
    clientes[clientesConectados] = *soclient;
    dirClientes[clientesConectados] = soclient;
    strcpy(nicknames[clientesConectados],DEFAULTNICK);
    clientesConectados++;
    
    /* Le enviamos el socket al hilo/moderador */
    pthread_create(&thread , &attr , moderador, (void *) soclient);
    

    /* El servidor puede hacer alguna tarea más o simplemente volver a esperar*/
  }
  

  /* Código muerto */
  close(socksrv);

  return 0;
}

void * moderador(void *_arg){

  int sock = *(int*) _arg
      ,indice
      ,contador = 0
      ,clienteOn = 1
      ,nickNameFlag = 0;

  char buf[BSIZE]
      ,nickname[BSIZE]
      ,auxiliar[BSIZE]
      ,nuevoNick[BSIZE];
  
  /*Cargamos el nombre por default*/
  strcpy(nickname,DEFAULTNICK);

  /*Obtenemos el indice correspon*/  
  indice = obtener_indice_cliente(sock);
  
  /*Pedimos el nickname por pirimera vez al cliente*/
  nickNameFlag = ask_nickname_1(sock,indice,nickname);

    
  clienteOn = nickname_request_manager(sock,indice,nickname,buf,nickNameFlag);
  
  if (clienteOn != EXIT && clienteOn != NOTIFICADO){

    create_msg_nuevo_nickname_notification(indice,nickname,buf);
    send_all(buf);

    imprimir_nicknames();

    strcpy(buf," ");

    pthread_mutex_unlock(&candado);    // Libera el candado que se encontraba tomado en el lock de ask_nickname2
    
    /*Le enviamos al cliente cual es su nickname en el server*/
    strcpy(buf,NICKNAME_OTORGADO);
    strcat(buf,nickname);
    send(sock , buf,sizeof(buf), 0);
    
  }
  

  while(clienteOn){

    /*Recibimos un mensaje del cliente y el evaluamos*/  
    recv(sock, buf, sizeof(buf), 0);
    clienteOn = procesar_mensaje(sock,buf,nickname);

  }
  
  /*Cerramos el socket y liberamos la memoria*/
  close(sock);
  free((int*)_arg);
  return NULL;
}

int obtener_indice_cliente(int sock){
    int indice;

    for(int i = 0; i < clientesConectados; i++){
        
        if(clientes[i]==sock){
            indice=i;
        }
    }

    return indice;
}

int ask_nickname_1(int sock,int indice,char nickname[BSIZE]){

  int ret = DISPONIBLE; 
  char aux[BSIZE];

  strcpy(aux,nickname);

  send(sock , NICKNAME_REQUEST,sizeof(NICKNAME_REQUEST), 0);
  recv(sock, aux, sizeof(aux), 0);

  pthread_mutex_lock(&candado); 
  
  ret = check_exit_msg(aux,indice,sock);

  if (ret != EXIT && is_nickname_disponible(aux) != DISPONIBLE){
      ret = NO_DISPONIBLE;
  }

  strcpy(nickname,aux);
    
  pthread_mutex_unlock(&candado); 

  return ret;

}

int ask_nickname_2(int sock,int indice,char nickname[BSIZE]){

  int clienteOn = 1;
  char aux[BSIZE];

  strcpy(aux,nickname);

  pthread_mutex_lock(&candado); 

  while (is_nickname_disponible(aux) != DISPONIBLE){
           
    pthread_mutex_unlock(&candado);

    send(sock ,NICKNAME_EN_USO , sizeof(NICKNAME_EN_USO), 0);
    
    recv(sock, aux, sizeof(aux), 0);

    pthread_mutex_lock(&candado);
        
    clienteOn = check_exit_msg(aux,indice,sock);
        
  }

  strcpy(nickname,aux);

  return clienteOn;    

}

int check_exit_msg(char nickname[BSIZE],int indice,int sock){

    int clienteOn = 1;

    if (verificar_operacion(nickname) == CLIENTE_DESCONECTAR){
        strcpy(nicknames[indice],nickname);
        desconectar_cliente(sock,nickname);
        clienteOn = EXIT;
    }

    return clienteOn;

}

void create_msg_nuevo_nickname_notification(int indice, char nickname[BSIZE], char buf[BSIZE]){

    strcpy(nicknames[indice],nickname);
    strcpy(buf,NICKNAME_NOTIFICATION_ALL);
    strcat(buf,nickname);  
}

int nickname_request_manager(int sock, int indice, char nickname[BSIZE],char buf[BSIZE], int nickNameFlag){
    
    int clienteOn = 1;

    switch (nickNameFlag){

      case EXIT:
      {
        clienteOn = 0;
        break;
      }
      case NO_DISPONIBLE:
      {
        clienteOn = ask_nickname_2(sock,indice,nickname);
        break;
      }

      default: // Recibimos nickname correcto
      {
        
        create_msg_nuevo_nickname_notification(indice,nickname,buf); 
        send_all(buf);
        clienteOn = NOTIFICADO;
        break;  
      }


  }
    
  return clienteOn;
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

void send_all(char buf[BSIZE]){
    
    char aux[BSIZE];
    strcpy(aux,buf);


    for(int i=0; i < clientesConectados; i++)

        /*Madamos el mensaje a todos aquellos que no tenengan el nombre
          por default*/
        if(strcmp(nicknames[i],DEFAULTNICK)!=0){
            send(clientes[i],aux,sizeof(aux), 0);
        }
      

}

int cambiar_nickname(int sock,char buf[BSIZE],char nickname[BSIZE]){
    
    char nuevoNickName[BSIZE];
    char msg[BSIZE];

    strcpy(msg,"");

    int lenNickname,ret=-1;

    if (obtener_nickname(nuevoNickName,buf) != -1){

        lenNickname = sizeof(nuevoNickName);
        int indice; 

        if(lenNickname > 0){

            if (is_nickname_disponible(nuevoNickName) == DISPONIBLE ){

                indice = indice_nickname(nickname);

                strcpy(nicknames[indice],nuevoNickName);
            
                strcpy(msg,"[ChatServer]");
                strcat(msg,nickname);
                strcat(msg,NICKNAME_NUEVO_OTORGADO2);
                strcat(msg,nuevoNickName);
                send_all(msg);

                strcpy(msg,"");    

                strcpy(nickname,nuevoNickName);
                
                strcat(msg,NICKNAME_NUEVO_OTORGADO1);
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

void mensaje_all(char buf[BSIZE],char nickname[BSIZE]){
  char buffer[BSIZE];
  
  /*Acomodamos el mensaje con el nickname del cliente*/
  strcpy(buffer,nickname);
  strncat(buffer," >> ",sizeof(" >> "));
  strcat(buffer,buf);

  send_all(buffer);

}

void clientes_fix_array(char nickname[BSIZE]){

    int indice = indice_nickname(nickname);
    clientesConectados--;
    int indiceUltimo = clientesConectados;

    printf("Cliente [%d:%s] se desconecta,\n",clientes[indice],nicknames[indice]);

    if (indice != indiceUltimo){

        printf("Cliente [%d:%s] reemplazo su lugar\n",clientes[indiceUltimo],nicknames[indiceUltimo]);
        strcpy(nicknames[indice],nicknames[indiceUltimo]);
        clientes[indice] = clientes[indiceUltimo];
    }

   
}

int desconectar_cliente(int sock,char nickname[BSIZE]){

    clientes_fix_array(nickname);
    imprimir_nicknames();
    send(sock,EXITMSG,sizeof(EXITMSG), 0);
    return 0;

}

int ejecutar_operacion(int sock,char buf[BSIZE], int operacion,char nickname[BSIZE]){

    int salir = 1;//Utilizamos salir cuando uno de los clientes se queire desconectar

    /*Dependiendon del valor de operacion realizamos alguna operacion del servidor*/
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
    
    /*Iniciamos el procesado del mensaje*/
    pthread_mutex_lock(&candado);
    op=ejecutar_operacion(sock,buff,op,nickname);
    pthread_mutex_unlock(&candado);
    return op;
}

int is_nickname_disponible(char nickname[BSIZE]){
    int ret=0;
        if(strcmp(nickname,DEFAULTNICK)!=0){
            ret=indice_nickname(nickname);
        }
    return ret;
}

void error(char *msg){
  exit((perror(msg), 1));
}

int indice_nickname(char nickname[BSIZE]){
    int longitud = clientesConectados;
    int ret = DISPONIBLE;

    for(int i = 0; i < longitud && ret == -1 ;i++){
        //printf("|%s|%s|%d\n",nicknames[i],nickname,strcmp(nicknames[i],nickname));
        if(strcmp(nicknames[i],nickname) == 0 && strcmp(nickname,DEFAULTNICK)!=0 ){
            ret=i;
            //printf("Se encontro un nombre igual\n");
        }
            
    }
    
    return ret;
}

int verificar_operacion(char buf[BSIZE]){
    char comando[15]="";
    char aux[BSIZE];
    char* token;
    int ret = MENSAJE_ALL;
    
    strcpy(aux,buf);
    token = strtok(aux," ");

    if (token != NULL){

        strcpy(comando,token);
    }

    /*Evaluamos que tipo de comando viene en el mensaje*/
    if(strcmp(comando,"/exit")==0){
        ret = CLIENTE_DESCONECTAR;
    }
    else if(strcmp(comando,"/msg")==0){
        ret = MENSAJE_PRIVADO;
    }
    else if(strcmp(comando,"/nickname")==0){
        ret = CLIENTE_NUEVO_NICK;

    }
    /*Retornamos el tipo de operacion a realizar*/
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
  if (bind(sock, (struct sockaddr *) &servidor, sizeof(servidor))){
    error("Error en el bind");
    close(sock);
  }
  return sock;
}

void imprimir_nicknames(){
    
    printf("[Nicknames]\n");
    for(int i = 0; i < clientesConectados; i++)
        printf("[Cliente:Nickname] [%d:%s]\n",clientes[i],nicknames[i]);
    printf("______________________________\n");

}

