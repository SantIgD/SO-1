#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
        
    }

    printArgumentos(sizeArgumentos,argumentos);

    return argumentos;

}



int main(int argc, char* argv[]){

    char argumento[SIZE];
    char* token;
    char** argumentos;

    
    printf("%s:$ ",argv[0]);
    scanf("%[^\n]s",argumento);
    printf("[argumentos] %s\n",argumento);
    
    argumentos = filtrar_argumentos(argumento);
    
    
    liberar_memoria(argumentos);
    
    return 0;
}


