#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>

#define N_FILOSOFOS 5
#define ESPERA 5000000

pthread_mutex_t izquierda;


pthread_mutex_t tenedor[N_FILOSOFOS];

void pensar(int i)
{
  printf("Filosofo %d pensando...\n",i);
  usleep(random() % ESPERA);
}

void comer(int i)
{
  printf("Filosofo %d comiendo...\n",i);
  usleep(random() % ESPERA);
}

void tomar_tenedores_d(int i)
{
  pthread_mutex_lock(&tenedor[i]); /* Toma el tenedor a su derecha */
  pthread_mutex_lock(&tenedor[(i+1)%N_FILOSOFOS]); /* Toma el tenedor a su izquierda */
}

void tomar_tenedores_i(int i)
{
  pthread_mutex_lock(&tenedor[(i+1)%N_FILOSOFOS]); /* Toma el tenedor a su izquierda */
  pthread_mutex_lock(&tenedor[i]); /* Toma el tenedor a su derecha */
}

void dejar_tenedores_d(int i)
{
  pthread_mutex_unlock(&tenedor[i]); /* Deja el tenedor de su derecha */
  pthread_mutex_unlock(&tenedor[(i+1)%N_FILOSOFOS]); /* Deja el tenedor de su izquierda */
}

void dejar_tenedores_i(int i)
{
  pthread_mutex_unlock(&tenedor[(i+1)%N_FILOSOFOS]); /* Deja el tenedor de su izquierda */
  pthread_mutex_unlock(&tenedor[i]); /* Deja el tenedor de su derecha */
}

void *filosofo(void *arg)
{
  int i = *(int*)arg;
  printf("Filosofo [%d]\n",i);

  while(1)
  {

    if(i%2==0){
      tomar_tenedores_d(i);
      
    }else{
      tomar_tenedores_i(i);
    }
    
    comer(i);

    if(i%2!=0){
      dejar_tenedores_i(i);
    }else{
      dejar_tenedores_d(i);
    }
    
    pensar(i);
  }
}

int main()
{
  int i;
  pthread_t filo[N_FILOSOFOS];
  for (i=0;i<N_FILOSOFOS;i++)
    pthread_mutex_init(&tenedor[i], NULL);
  
  
  int args[5];
  for (i=0;i<N_FILOSOFOS;i++){
    args[i]=i;
    pthread_create(&filo[i], NULL, filosofo, (void *)&args[i]);
  }
  pthread_join(filo[0], NULL);
  return 0;
}
