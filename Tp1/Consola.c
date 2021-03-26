#include <stdio.h>
#include <unistd.h>
#include <wait.h>
#include <stdlib.h>
#include <string.h>

#define SIZE 1024
#define TERMINAR 10

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

    return contador;

}

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

char** concatenar_argumento(int sizeArgumentos,int sizeArgumento, char** argumentos, char* argumento ){

    argumentos = asignar_size(sizeArgumentos,sizeArgumento,argumentos);
    
    if (argumento != NULL){
        argumentos[sizeArgumentos - 1] = strcpy(argumentos[sizeArgumentos - 1] ,argumento);
    }else{
        argumentos[sizeArgumentos - 1] = NULL;
    }
    
    return argumentos;

}

char** filtrar_argumentos(char* argumento){
    
    int sizeArgumentos = 1;
    char** argumentos = NULL;
    char* token;

    token = strtok(argumento, " ");
    
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

    printArgumentos(sizeArgumentos,argumentos);

    return argumentos;

}

int simulacion_consola(char** argv){
    
    char argumento[SIZE];
    char** argumentosOurs;
    int cantArgumentos,r;

    
    printf("%s:$ ",argv[0]);
    scanf("%[^\n]s",argumento);
    printf("[argumentos] %s\n",argumento);
    
    argumentosOurs = filtrar_argumentos(argumento);
    
    
    
    cantArgumentos = 2//cantidad_elementos_charpp(argumentosOurs);
    char argumentosSO[cantArgumentos][100];
    //char** arg = (char **) argumentosSO;

    

    
    
    charppcpy(argumentosSO,argumentosOurs,cantArgumentos);
    //printArgumentos(cantArgumentos,argumentosSO);
    //arg[cantArgumentos] = NULL;
    //printArgumentos(cantArgumentos,arg);
    
    r=execv(argumentosSO[0],argumentosSO);
    liberar_memoria(argumentosOurs);
    printf("%d\n",r);

    
}

/* /home/charles_chaplin/Desktop/SOI/SO-1/Clase2/Ejemplos/Exec/HelloPid
char** entradaConsola(char** argumentos){
    
    printf("%s $",argv[0]);
    scanf("%s",&argumentos);

    argumentos = filtrar_argumentos(argumentos);
    
    return argumentos;
}
*/

int main(int argc, char*argv[]){

    pid_t forkOut;
    int status=0;
    simulacion_consola(argv);
    /*while (status != TERMINAR){
        
        forkOut = fork();
        
        if (forkOut < 0){ // ERROR
            perror("Ha ocurrido un error salvaje! ");
        }else if (forkOut == 0){ // hijo, consola

            simulacion_consola();

        _exit()
        }else{
            
            wait(&status); // para exit definitivo
        }

    }
    */    
    return 0;
}


