#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>


int main(){

    char texto[1024]="hola";
    int i=1; 
    while(i++){
        sleep(1);
        for(int j=i;j!=0;j--)
            strcat(texto,"!\0");
        assert( 0 < strlen(texto));
        write(1,texto,strlen(texto));
        strcpy(texto,"hola\0");
    }

    return 0;
}