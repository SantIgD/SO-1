#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

int main(int argc, char** argv){
    
    if (argc < 3){
        printf("Faltan entradas\n ");
    } else {
        char* args[] = {NULL};
        pid_t forkOut;
        int tiempo = atoi(argv[2]);
        char* nombreArchivoFuente = argv[0];
        char* pathArchivoDestino = argv[1];
        int execvOut;

        while (1){
            
            forkOut = fork();

            if (forkOut < 0){
                perror ("Ocurrio un error!");
            } else if (forkOut == 0){
                
                
                execvOut=execv(pathArchivoDestino,args);
            }else{
                sleep(tiempo);
            }
        }
        
    }

    return 0;
}