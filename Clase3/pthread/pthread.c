#include<pthread.h>
#include<stdio.h>
#include<assert.h>

void* printEcho(void *i){
  /* Interpretamos la entrada como un entero */
  int arg = *((int*)i);
  /* Mostramos un mensaje */
  printf("ECHO:%d!\n",arg);

  return NULL;
}

int main(int argc,char **argv){
  pthread_t thread_id[2];
  int arg1=1, arg2=2;
  void *res;

  printf("Creamos dos hilos!\n");

  /* Creamos dos hilos */
  assert(! pthread_create(&thread_id[0], NULL, printEcho, (void*)&arg1));
  assert(! pthread_create(&thread_id[1], NULL, printEcho, (void*)&arg2));

  printf("Esperamos a que terminen...\n");

  /* Esperamos a que terminen */
  assert(! pthread_join(thread_id[0], &res));
  assert(! pthread_join(thread_id[1], &res));

  printf("Terminamos!\n");

  return 0;
}
