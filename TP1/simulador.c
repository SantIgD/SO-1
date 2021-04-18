#include <stdio.h>
#include "Game.h"






int main(int argc, char** argv){

    if (argc > 1){

        game_t* juego = game_create();
        
        game_init(juego,argv[1]);
        game_show(juego);
    }
    return 0;
}