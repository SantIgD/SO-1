#include <stdio.h>
#include "Game.h"



void cargar(){
    char nombreArchivo[1024];

    printf("Nombre del archivo: ");
    scanf("%s",nombreArchivo);
    game_t* juego = loadGame(nombreArchivo);

    
}


int main(){

    cargar():

    return 0;
}