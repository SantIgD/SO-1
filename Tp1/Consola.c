#include <stdio.h>
#include <unistd.h>
#include <wait.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 1024
#define TERMINAR 10

void liberar_memoria(char** argumentos){

    for(int i = 0; argumentos[i+1] != NULL; i++){
        free(argumentos[i]);
    }
    
    free(argumentos);

}

void charppcpy(char dest[][100],char** source ,int sizeArgumentos){

    printf("[size] %d\n",sizeArgumentos);

    for(int i=0; i < sizeArgumentos; i++){

        strcpy(dest[i],source[i]);
        printf("[%d] [source] %s [dest] %s\n",i,source[i],dest[i]);
    }
    
}

int cantidad_elementos_charpp(char** argumentos){

    int contador=0;

    for (contador; argumentos[contador] != NULL; contador++){

    }

    return (contador+1);

}

void printArgumentos(int sizeArgumentos, char** argumentos){


    for (int i =0; i < sizeArgumentos; i++){

            printf("[%d]%s\n",i,argumentos[i]);
        }
        
}

char** asignar_size(int sizeArgumentos, int lenArgumento, char** argumentos){

    int indiceArgumento = sizeArgumentos - 1;

    argumentos = realloc(argumentos,sizeof(char*) * sizeArgumentos);

    argumentos[indiceArgumento] = realloc(argumentos[indiceArgumento], sizeof(char*) * lenArgumento);

    return argumentos;

}

char** concatenar_argumento(int sizeArgumentos,int sizeArgumento, char** argumentos, char* entradaTerminal ){

    argumentos = asignar_size(sizeArgumentos,sizeArgumento,argumentos);
    
    if (entradaTerminal != NULL){
        argumentos[sizeArgumentos - 1] = strcpy(argumentos[sizeArgumentos - 1] ,entradaTerminal);
    }else{
        argumentos[sizeArgumentos - 1] = NULL;
    }
    
    return argumentos;

}

char** estructurar_argumentos(char* entradaTerminal){
    
    int sizeArgumentos = 1;
    char** argumentos = NULL;
    char* token;

    token = strtok(entradaTerminal, " ");
    
    if (token != NULL){

        argumentos=concatenar_argumento(sizeArgumentos,strlen(token),argumentos,token);

        token = strtok(NULL, " ");

        while(token != NULL){
            
            sizeArgumentos++;
            argumentos=concatenar_argumento(sizeArgumentos,strlen(token),argumentos,token);
            token = strtok(NULL, " ");

        }

        sizeArgumentos++;
        argumentos=concatenar_argumento(sizeArgumentos,0,argumentos,NULL);
        
    }else{

        argumentos=concatenar_argumento(sizeArgumentos,0,argumentos,NULL);
    }

    //printArgumentos(sizeArgumentos,argumentos);

    return argumentos;

}

void leer_linea(char destino[SIZE],char** argv){

    printf("%s:$ ",argv[0]);

    fflush( stdin );
    //fgets(destino,SIZE,stdin);
   
    scanf(" %[^\n]s",destino);
  
    //printf("[argumentos] %s\n",destino);

}

int isChild(pid_t forkOut){
    int response=0;
    
    if (forkOut == 0){
        response++;
    }
        
    return response;
}

int isForkError(pid_t forkOut){
    int response=0;
    
    if (forkOut == -1){
        response++;
    }
        
    return response;
}

int isExecError(int result){
    int response=0;
    
    if (result == -1){
        response++;
    }
        
    return response;
}

int ampersandBelong(char* argumento){
    int response = 0;

    if (!strcmp(argumento,"&")){
        response++;
    }
    return response;
}

int isTerminar(char* argumento){
    
    int response = 0;

    if (!strcmp(argumento,"&")){
        response++;
    }
    return response;
}

int simulacion_consola(char** argv){
    
    char entradaTerminal[SIZE];
    char** argumentos;
    int r,status=0,cantArgumentos=0;
    pid_t forkOut;


    while(1){

        //Parent
        leer_linea(entradaTerminal,argv);
        argumentos = estructurar_argumentos(entradaTerminal);
        cantArgumentos = cantidad_elementos_charpp(argumentos);

        forkOut=fork();

        if (isForkError(forkOut)){

            perror("Error fork");

        }else if(isChild(forkOut)){

            r=execv(argumentos[0],argumentos);
        
            if(isExecError(r)){
                liberar_memoria(argumentos);
                perror("Error exec");
                _exit(0);
            }
            
        }else{
            // parent
            //printf("[______]%d\n",ampersandBelong(argumentos[cantArgumentos-2]));
            if (!ampersandBelong(argumentos[cantArgumentos-2])){ // EL ultimo es NULL
                //printf("No exploto todavia\n");
                wait(&status);
            }
            
            strcpy(entradaTerminal,"");
        }

    }

    liberar_memoria(argumentos);
    
    return 0;
    
}


/* /home/charles_chaplin/Desktop/SOI/SO-1/Clase2/Ejemplos/Exec/HelloPid */


int main(int argc, char*argv[]){

    return simulacion_consola(argv);

}


