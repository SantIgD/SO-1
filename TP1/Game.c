#include "Game.h"
#include <pthread.h>
#include "Barreras.h"
#include <stdio.h>

/******************************************************************************/
/* Representamos las células vivas como 'O' y las muertas como 'X' */
/******************************************************************************/
struct _game{

    board_t* board;
    int ciclos;

};

pthread_mutex_t candado;

/******************************************************************************/
 
game_t * game_create(){
    
    return malloc(sizeof(game_t));
  
}


int game_init(game_t* game, char* filename){
    
    game->board = board_create();

    game_load(game,filename);
  
}

/* Cargamos el juego desde un archivo */
int game_load(game_t* game, char *filename){

    int filas,columnas;
    FILE* archivo = fopen(filename,"r");

    if(archivo == NULL){
        perror("No se pudo abrir el archivo! ");
    }

    fscanf(archivo, "%d %d %d",&game->ciclos,&filas,&columnas);
    fclose(archivo);

    board_cells_create(game->board,filas,columnas);

    board_init(game->board,filename);

    

    return 0;

}

/* Guardamos el tablero 'board' en el archivo 'filename' */
void game_writeBoard(board_t board, const char *filename);

/* Simulamos el Juego de la Vida de Conway con tablero 'board' la cantidad de
ciclos indicados en 'cycles' en 'nuprocs' unidades de procesamiento*/
board_t *congwayGoL(board_t *board, unsigned int cycles, const int nuproc);
 

void game_show(game_t* game){
    board_show(game->board);
}