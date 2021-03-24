#include<stdio.h>
#include<unistd.h>
#include<wait.h>

int main(int argc, char**argv){

  pid_t forkOut;

  forkOut = fork();

  if (forkOut < 0) {
    perror("Error!");
  }

  if (forkOut == 0){ /* Child */
    printf("[ Child ] %d \n ", getpid());
    printf("A mirmir\n");
    sleep(2);
   
    _exit(0x1ff);

  } else { /* Parent */
    int status;

    printf("[Parent] %d\n", getpid());
    wait(&status);

    /* WIFEXITED= devuelve true si el child vuelve correctamente */
    if (WIFEXITED(status)) {
      /* WEXITSTATUS= devuelve los ultimos 8 bits de el status del child
      si WIFEXITED devuelve true */
      printf("Child resultó en %d\n",WEXITSTATUS(status));
    } else {
      printf("WHAAAT \n");
    }
  }

  return 0;
}
