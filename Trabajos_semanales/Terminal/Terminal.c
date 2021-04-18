#include <stdio.h>
#include <unistd.h>
#include <wait.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 1024


void liberar_memoria(char** argumentos){
    int i = 0;

    for(i; argumentos[i] != NULL; i++){
        free(argumentos[i]);
    }
    
    
    free(argumentos);

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
    
    if (lenArgumento!=0){
        argumentos[indiceArgumento] = malloc (sizeof(char*) * lenArgumento);
    }
    return argumentos;

}

char** concatenar_argumento(int sizeArgumentos, char** argumentos, char* entradaTerminal ){

    int sizeArgumento;
    
    if (entradaTerminal != NULL){

        sizeArgumento = strlen(entradaTerminal);
        argumentos = asignar_size(sizeArgumentos,sizeArgumento,argumentos);
        argumentos[sizeArgumentos - 1] = strcpy(argumentos[sizeArgumentos - 1] ,entradaTerminal);
        
    }else{
        
        argumentos = asignar_size(sizeArgumentos,0,argumentos);
        argumentos[sizeArgumentos - 1] = NULL;
    }
    
    return argumentos;

}

char** estructurar_argumentos(char* entradaTerminal){
    
    int sizeArgumentos = 0;
    char** argumentos = NULL;
    char* token;

    token = strtok(entradaTerminal, " ");
    
    sizeArgumentos++;
    //printf("%s\n",token);
    argumentos=concatenar_argumento(sizeArgumentos,argumentos,token);

    token = strtok(NULL, " ");

    while(token != NULL){
        
        sizeArgumentos++;
        argumentos=concatenar_argumento(sizeArgumentos,argumentos,token);
        token = strtok(NULL, " ");

    }
    


    sizeArgumentos++;

    argumentos=concatenar_argumento(sizeArgumentos,argumentos,NULL);
    

    //printArgumentos(sizeArgumentos,argumentos);

    return argumentos;

}

void leer_linea(char destino[SIZE],char** argv){

    printf("%s:$ ",argv[0]);

    fflush( stdin );

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

int isTerminar(char** argumentos,int sizeArgumentos){
    
    int response = 0;

    //printf("[arg0] %s [sizeArgs] %d",argumentos[0],sizeArgumentos);

    if (!strcmp(argumentos[0],"exit") && sizeArgumentos == 2){ // El ultimo es NULL
        response++;
        
    }
    return response;
}

void concurrencia(pid_t forkOut,char**argumentos,int cantArgumentos,char* entradaTerminal){

    int status=0, // retorno del child
        r; // retorno de execv

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

int simulacion_terminal(char** argv){
    
    char entradaTerminal[SIZE];
    char** argumentos;
    int cantArgumentos=0;
    pid_t forkOut;
    int terminar=0; //bandera para salir de la terminal
    
    while(!terminar){
        
        leer_linea(entradaTerminal,argv);

        argumentos = estructurar_argumentos(entradaTerminal);

        cantArgumentos = cantidad_elementos_charpp(argumentos);

        terminar = isTerminar(argumentos,cantArgumentos);

        if (!terminar){
            
            forkOut=fork();
            concurrencia(forkOut,argumentos,cantArgumentos,entradaTerminal);
            
        }     
        liberar_memoria(argumentos);
    }
    
    
    
    return 0;
    
}


/* /home/charles_chaplin/Desktop/SOI/SO-1/Clase2/Ejemplos/Exec/HelloPid */


int main(int argc, char*argv[]){

    return simulacion_terminal(argv);

}


