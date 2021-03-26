#include<unistd.h>
#include<stdio.h>
#include<stdlib.h>

int main(int argc, char **argv){
  pid_t p; // Int, pero usar los tipos, dijieron que van a estar exigentes
  
  /* Se comparte */
  p = fork();

  /* | | */
  if (p < 0) {
    perror(" Erro! ");
  }
  printf("\n[Proceso %d] ", getpid());

  if (p == 0){ /* Child */
    printf("\nHola! Soy Child con PID: %d \n", getpid());
    _exit(0);
  } else { /* Parent */
    printf("\nHola! Soy Parent con PID: %d y Child es %d \n", getpid(),p);
  }
  
  return 0;
}
