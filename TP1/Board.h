#ifndef __BOARD_H_
#define __BOARD_H_

#include <stdlib.h>
#include <stdio.h>
/******************************************************************************/
/* Definición de la estructura de datos del tablero */

struct _board;
typedef struct _board board_t;
/******************************************************************************/
/******************************************************************************/

/* Funciones sobre el tablero */

/* Creación del tablero */
board_t* board_create();

/* Creación de celdas */
int board_cells_create(board_t* board,int row,int col);

/* X->1, O->0*/
int get_state(char val);

/* Asignarle cant veces 'val' de manera consecutiva a la posición (col, row), (col+1, row),..., (row,col+cant) del tablero*/
board_t* board_set(board_t* board,int row,int col,int cant,char val);

/* Leer de una lista de caracteres que codifican un tablero en formato RLE e
 * interpretarla como una fila del tablero */
int board_load(board_t *board, char *str, int row);

/* Inicialización del tablero */
int board_init(board_t *board, char* filename);

/* Creación del tablero con un elemento por default*/
//int board_init_def(board_t *board, size_t col, size_t row, char def);

/* Leer el tablero en una posición (col, row) */
//char board_get(board_t board, unsigned int col, unsigned int row);

/* Leer el tablero en una posición asumiendo que el tablero es 'redondo'.*/
char board_get_round(board_t board, int col, int row);


/* Función para mostrar el tablero */
/* La función 'board_show' imprime el board.*/
void board_show(board_t* board);

/* Destroy board */
//void board_destroy(board_t *board);
#endif
