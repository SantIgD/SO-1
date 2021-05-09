#include <stdlib.h>
#include <stdio.h> /* Print stuff */
#include <unistd.h> /* Sleep */
#include <signal.h> /* Signals */

void handler(int arg){
  static int d = 0;

  if (d < 2){
    printf("Ouch %d \n", d);
    d ++ ;
  } else {
    printf("No doy ma \n");
    exit(0);
  }

  return;
}

int main(){

  /* Cambiar el comportamiendo de una señal  */
  signal(SIGINT, handler);

  while(1){
    printf("Holis\n");
    sleep(1);
  }

  return 0;
}
