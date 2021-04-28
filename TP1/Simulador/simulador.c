#include <stdio.h>
#include "Game.h"
#include <sys/sysinfo.h>


int main(int argc, char** argv){

    if (argc > 1){

        game_t* juego = game_create();
        
        game_init(juego,argv[1]);

        congwayGoL(juego,get_nprocs());

        game_writeBoard(juego,argv[1]);
        
        game_destroy(juego);
    }

    return 0;
}