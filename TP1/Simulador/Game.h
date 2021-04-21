#ifndef GAME_TYPES
#define GAME_TYPES
#include "Board.h"

/******************************************************************************/
/* Representamos las células vivas como 'O' y las muertas como 'X' */

enum State {ALIVE, DEAD};

/******************************************************************************/
/* Definición de la estructura de datos del juego */
/******************************************************************************/

struct _game;

typedef struct _game game_t;
/******************************************************************************/

/******************************************************************************/
/* Funciones sobre el juego */
/******************************************************************************/

/* Creamos el objeto juego */
game_t * game_create();

/* Inicializa el objeto juego */
int game_init(game_t* game, char* filename);

/* Carga el juego desde un archivo */
int game_load(game_t* game, char *filename);

/* Guarda el juego en un archivo de salida */
void game_writeBoard(game_t* game, char *filename);

/* Destruye el juego */
void game_destroy(game_t* game);

/* Simula el Juego de la Vida de Conway con tablero 'board' la cantidad de
ciclos indicados en 'cycles' (ambos en la estructura game) y en 'nuprocs' la cantidad de hilos disponibles*/
int congwayGoL(game_t *game , const int nuproc);

/* Guarda el juego en un archivo de salida */
int game_getCantFilas(game_t* game);

/* Guarda el juego en un archivo de salida */
int game_getCantColumnas(game_t* game);

/* Función para mostrar el juego (lo imprime en pantalla)*/
void game_show(game_t* game);

/* Verifica si el vecino superior en el juego esta vivo o no*/
int game_vecino_superior(game_t *game,int row,int col);

/* Verifica si el vecino superior izquierdo en el juego esta vivo o no*/
int game_vecino_superior_i(game_t *game,int row,int col);

/* Verifica si el vecino superior derecho en el juego esta vivo o no*/
int game_vecino_superior_d(game_t *game,int row,int col);

/* Verifica si el vecino inferior en el juego esta vivo o no*/
int game_vecino_inferior(game_t *game,int row,int col);

/* Verifica si el vecino inferior izquierdo en el juego esta vivo o no*/
int game_vecino_inferior_i(game_t *game,int row,int col);

/* Verifica si el vecino inferior inferior en el juego esta vivo o no*/
int game_vecino_inferior_d(game_t *game,int row,int col);

/* Verifica si el vecino lateral izquierdo en el juego esta vivo o no*/
int game_vecino_lateral_i(game_t *game,int row,int col);

/* Verifica si el lateral izquierdo en el juego esta vivo o no*/
int game_vecino_lateral_d(game_t *game,int row,int col);

/* Función para mostrar el juego (lo imprime en pantalla)*/
void game_set_value(game_t* game, int row, int col, int value);

/******************************************************************************/
#endif
