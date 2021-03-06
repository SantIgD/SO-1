#ifndef __BOARD_H_
#define __BOARD_H_

#include <stdlib.h>
#include <stdio.h>

//NOTA : tablero o board.

/******************************************************************************/
/* Definición de la estructura de datos del tablero */
/******************************************************************************/

struct _board;
typedef struct _board board_t;

/******************************************************************************/

/******************************************************************************/
/* Funciones sobre el tablero */
/******************************************************************************/

/* Crea el objeto tablero */
board_t* board_create();

/* Crea las celdas de una tablero */
int board_cells_create(board_t* board,int row,int col);

/* Lee de una lista de caracteres en formato RLE que codifican una fila del tablero  e
 * interpretarla como una fila del tablero */
int board_load(board_t *board, char *str, int row);

/* Inicializa el tablero */
int board_init(board_t *board, char* filename);

/* Escribe el tablero en un archivo de salida.*/
void board_write(board_t* board, char* filename);

/* Destruye el tablero */
void board_destroy(board_t *board);

/* Asigna cant veces 'val' desde la columna cow de manera consecutiva a la fila row del tablero*/
int board_actual_set(board_t* board,int row,int col,int cant,char val);

/* Asigna el valor valor a la siguiente generacion del tablero en la posicion en la fila row y columna row*/
int board_proxGen_set(board_t* board,int row,int col,int valor);

/* Actualiza el tablero a la proxima generacion*/
void board_interchange(board_t* board);

/* Lee el tablero en la fila row y columna col */
int board_get(board_t* board, int row, int col);

/* Devuelve la cantidad de filas del tablero*/
int board_getCantFilas(board_t * board);

/* Devuelve la cantidad de columnas del tablero*/
int board_getCantColumnas(board_t * board);

/* Función para mostrar el tablero (lo imprime en pantalla)*/
void board_show(board_t* board);

/******************************************************************************/
#endif
