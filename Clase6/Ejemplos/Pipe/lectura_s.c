#include <stdio.h>
#include <unistd.h>
#include <string.h>
#define BUFFER 1024

int main(){
  char buff[BUFFER];
  ssize_t num;
  int i = 0;
  while(76){

  printf("[Read] Esperando lectura...\n");
  //scanf("%d",&num);
  num = read(0,buff,BUFFER);
  buff[num+1] = 0;
  //strcat(buff,"\0");

  printf("La palabra escrita es: %s %d %lu\n", buff, i++, num);
  
  sleep(1);
  }

    return 0;     
}
