#include<stdio.h> /* Printfs */
#include<unistd.h> /* getpid */
#include<stdlib.h>

int main(int argc, char **argv) {

  printf("Hola! Pid: %d\n",getpid());
  printf("A mimir\n");
  sleep(6);
  printf("Il retorno\n");
  return 0;
}
