#include<unistd.h> /* exec */
#include<stdio.h> /* printf */
#include<stdlib.h> /* Null */


/*Error*/


int main (int argc, char** argv){
  char *args[] = {"/usr/bin/exa", "-la" , NULL};
  int res = 0;

  printf("Hola soy programa %s con PID:%d\n", argv[0], getpid());

  res = execv("/usr/bin/exa", args);

  /* Código Muerto */

  if( res < 0){
    perror("Algo pasó!");
  }

  return 0;
}
