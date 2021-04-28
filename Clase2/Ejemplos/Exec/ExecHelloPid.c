#include<unistd.h> /* exec */
#include<stdio.h> /* printf */
#include<stdlib.h> /* Null */


/*Error*/


int main (int argc, char** argv){
  char *args[] = {"./HelloPid", NULL};
  int res = 0;

  printf("Hola soy programa %s con PID:%d\n", argv[0], getpid());

  res = execv("./HelloPid",args);
    /*1st Argument: Direccion
      2nd Argument: Argumenetos (El ultimo debe ser NULL y 
      el primero debe coincidir el nombre del archivo que se quiere ejecutar)*/
  
  /* Código Muerto */

  if( res < 0){
    perror("Algo pasó!");
  }

  return 0;


  
}
