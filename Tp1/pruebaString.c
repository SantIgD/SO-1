#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <wait.h>
#define SIZE 1024


char** asignar_size(int sizeArgumentos, int lenArgumento, char** argumentos){

    int indiceArgumento = sizeArgumentos - 1;

    argumentos = realloc(argumentos,sizeof(char*) * sizeArgumentos);

    argumentos[indiceArgumento] = realloc(argumentos[indiceArgumento], sizeof(char*) * lenArgumento);

    return argumentos;

}

void liberar_memoria(char** argumentos){

    for(int i = 0; argumentos[i+1] != NULL; i++){
        free(argumentos[i]);
    }
    

    free(argumentos);

}

void printArgumentos(int sizeArgumentos, char** argumentos){

    for (int i =0; i < sizeArgumentos; i++)
            printf("[%d]%s\n",i,argumentos[i]);
        
}

char** filtrar_argumentos(char* argumento){
    int sizeArgumentos = 1;
    int indiceArgumento = sizeArgumentos - 1;
    char** argumentos = NULL;
    char* token;

    token = strtok(argumento, " ");
    
    if (token != NULL){

        

        argumentos = asignar_size(sizeArgumentos,strlen(token),argumentos);
        argumentos[indiceArgumento] = strcpy(argumentos[indiceArgumento] ,token);


        token = strtok(NULL, " ");
        

        while(token != NULL){
            
            sizeArgumentos++;
            indiceArgumento = sizeArgumentos - 1;
            argumentos = asignar_size(sizeArgumentos,strlen(token),argumentos);
            argumentos[indiceArgumento] = strcpy(argumentos[indiceArgumento],token);
            token = strtok(NULL, " ");

        }

        sizeArgumentos++;
        indiceArgumento = sizeArgumentos - 1;
        argumentos = asignar_size(sizeArgumentos,0,argumentos);
        argumentos[indiceArgumento] = NULL;
        
    }

    printArgumentos(sizeArgumentos,argumentos);

    return argumentos;

}


int main(int argc, char* argv[]){

    char argumento[SIZE];
    char* token;
    char** argumentos;
    int out,r;
    char *args[] = {"./pruebaString", "&", NULL};
    
    while(1){
         
    printf("\n%s:$ ",argv[0]);
    fgets(argumento,SIZE,stdin);

    printf("[argumentos] %s\n",argumento);
    
    argumentos = filtrar_argumentos(argumento);
    r=fork();
    if(r==0){
        printf("estoy en el hijo\n");
        printArgumentos(3,argumentos);
        r=execv(argumentos[0],argumentos);
          if( r < 0){
            perror("Algo pasó!");
         }

    }else{
        wait(NULL);
        printf("termine");

        }
    }
    
    liberar_memoria(argumentos);
    
    return 0;
}




