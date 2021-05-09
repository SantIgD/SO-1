#include<stdlib.h>
#include<stdio.h>
#include<unistd.h>
#include <string.h>

#define READ 0
#define WRITE 1
int main(int argc, char* argv[]) {

 pid_t pid;
 int fd[2];
 if (pipe(fd) == -1) {
  perror("Creating pipe");
  exit(EXIT_FAILURE);
 }
 pid = fork();
 if (pid == 0){                                                                                                                                                                                                                                        // The child process will execute wc.
  //Esto es ejecutado por el hijo
  close(fd[WRITE]);//Se cierra el descriptor de escritura del pipe
  dup2(fd[READ], STDIN_FILENO); //Apunta al descriptor que devuelve el pipe 
  char *cm[3]; 
  cm[2] = NULL;
  // se hace uso de la funcion split para separar por palabras el comando y los argumentos
  split(argv[2], cm);
  //Se ejectua el comando con sus argumentos y la entrada que retorna el padre
  execvp(cm[0], cm);
 }
 else if (pid == -1){
  perror("fork() failed");
  exit(EXIT_FAILURE);
 }
 else{
  //Esto es ejecutado por el padre
  close(fd[READ]); // se cierra el descriptor de lectura del pipe
  dup2(fd[WRITE], STDOUT_FILENO); //Redirecciona la salida de escritura al pipe
  char *comand[3]; 
  comand[2] = NULL;
  split(argv[1], comand);
  //Se ejecuta el comando con los argumentos
  execvp(comand[0], comand);
 }
}