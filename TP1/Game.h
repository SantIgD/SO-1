#ifndef GAME_TYPES
#define GAME_TYPES
#include "Board.h"

/******************************************************************************/
/* Representamos las células vivas como 'O' y las muertas como 'X' */

enum State {ALIVE, DEAD};
/******************************************************************************/
struct _game;

typedef struct _game game_t;
/******************************************************************************/

/* Creamos el objeto juego */
game_t * game_create();

/* Inicializamos el objeto juego */
int game_init(game_t* game, char* filename);

/* Cargamos el juego desde un archivo */
int game_load(game_t* game, char *filename);

/* Guardamos el tablero 'board' en el archivo 'filename' */
void game_writeBoard(game_t* game, char *filename);

/* Simulamos el Juego de la Vida de Conway con tablero 'board' la cantidad de
ciclos indicados en 'cycles' en 'nuprocs' unidades de procesamiento*/
int congwayGoL(game_t *game , const int nuproc);

void game_show(game_t* game);

void game_destroy(game_t* game);

#endif