#include "Board.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/******************************************************************************/
/* Definición de la estructura de datos del tablero */

struct _board{

    int** tableroActual;
    int** tableroProxGen;
    int cantColumnas;
    int cantFilas;
};

/******************************************************************************/
/******************************************************************************/

/* Funciones sobre el tablero */

/* Creación del tablero */
board_t* board_create(){
    return malloc(sizeof(board_t));
}

int board_cells_create(board_t* board,int row,int col){

    board->cantColumnas = col;
    board->cantFilas = row;
    
    //printf("Col:%d Fil:%d", board->cantColumnas,board->cantFilas);
    //getchar();
    board->tableroActual = malloc(sizeof(int*) * board->cantFilas );
    board->tableroProxGen = malloc(sizeof(int*) * board->cantFilas );

    for (int i = 0; i < board->cantFilas; i++){
        board->tableroActual[i] = malloc(sizeof(int) * board->cantColumnas );
        board->tableroProxGen[i] = malloc(sizeof(int) * board->cantColumnas );
    }


    return 0;
}


int get_state(char val){

    int estado = 0;

    if (val=='X'){
        estado = 1;

    }
    
    return estado;
}


/* Asignarle cant veces 'val' de manera consecutiva a la posición (col, row), (col+1, row),..., (row,col+cant) del tablero*/
int board_actual_set(board_t* board,int row,int col,int cant,char val){

    int estado = get_state(val);

    for(int j = 0; j < cant; j++){
        
        board->tableroActual[row][col+j]=estado;
    }
   

    //printf("Salgo de set\n");
    //getchar();

    return 0;
}


int board_proxGen_set(board_t* board,int row,int col,int valor){


    board->tableroProxGen[row][col]=valor;   

    //printf("Salgo de set\n");
    //getchar();

    return 0;
}

/* Leer de una lista de caracteres que codifican un tablero en formato RLE e
 * interpretarla como una fila del tablero */
int board_load(board_t *board, char *str,int row){
    
    int columna=0 , cantidad=0;
    printf("[string] %s\n",str);
    int longitud = strlen(str);
     printf("[longitud] %d\n",longitud);
    for(int i = 0; i < longitud; i=i+2){

        cantidad = atoi((const char* ) &str[i]);
       // printf("[cantidad] %d, [Char] %c, [Siguiente] %c  \n ",cantidad,str[i],str[i+1]);
        board_actual_set(board,row,columna,cantidad,str[i+1]);
        //getchar();
        //printf("%d,%d\n", valor,columna);
        

        columna += cantidad;

    }

    
    printf("cantidad de columnas: %d /cantidad de columnas-tablero: %d //cantidad de fila: %d\n\n",columna, board->cantColumnas,row);
    if (columna != board->cantColumnas){

        perror("No se ingreso el numero de columnas correctas ");
        exit(EXIT_FAILURE);
    }

    return 0;
}

/* Inicialización del tablero */
int board_init(board_t *board,char* filename){
    
    int cantidadLetras = (board->cantColumnas)*2+2; // los pares NX y el final de linea \n\0
    char* linea=malloc(sizeof(char) * cantidadLetras);
    int fila=0;

    FILE* archivo = fopen(filename,"r");

    
    if(archivo == NULL){
        perror("No se pudo abrir el archivo! ");
    }

    fgets(linea,cantidadLetras,archivo);

    while(!feof(archivo)){

        fgets(linea,cantidadLetras,archivo);
        //printf("fila:%s",linea);

        //getchar();
        board_load(board,linea,fila);

        fila++;


    }
    
    
    if (fila != board->cantFilas){

        perror("No se ingreso el numero de filas correctas ");
        exit(EXIT_FAILURE);
    }
        

    fclose(archivo);
    return 0;
}

/* Creación del tablero con un elemento por default*/
//int board_init_def(board_t *board, size_t col, size_t row, char def);

/* Leer el tablero en una posición (col, row) */
int board_get(board_t* board, unsigned int row, unsigned int col){
    return board->tableroActual[row][col];
}

/* Leer el tablero en una posición asumiendo que el tablero es 'redondo'.*/
//char board_get_round(board_t board, int col, int row);


/* Función para mostrar el tablero */
/* La función 'board_show' imprime el board.*/
void board_show(board_t* board){

    for(int i =0; i < board->cantFilas; i++){

        printf("   ");

        for(int j = 0; j < board->cantColumnas; j++){

            //printf("%d",board->tableroActual[i][j] );
            
            if(board_get(board,i,j) == 0){
                printf("|O");
            }    
            else{
                printf("|X");
            }
            
        }
        if (i<board->cantFilas)
            printf("|\n");

    }

}

void board_interchange(board_t* board){
    
    int** aux = board->tableroProxGen;
    board->tableroProxGen = board->tableroActual;
    board->tableroActual = aux;

}

int board_getCantFilas(board_t * board){
    return board->cantFilas;
}


int board_getCantColumnas(board_t * board){
    return board->cantColumnas;
}

void board_write(board_t* board, char* filename){

    FILE* archivo = fopen(filename,"w");

    for(int i = 0; i < board->cantFilas; i++){
        
        for(int j = 0; j < board->cantColumnas; j++){
            
            if(board_get(board,i,j) == 1)
                fputc('X', archivo);
            else
                fputc('O', archivo);

        }
            
        fputc('\n', archivo);
    }
    fclose(archivo);

}

/* Destroy board */
void board_destroy(board_t *board){

    

    for (int i = 0; i < board->cantFilas; i++){
        free(board->tableroActual[i]);
        free(board->tableroProxGen[i]);
    }
    free(board->tableroActual);
    free(board->tableroProxGen);

    free(board);
}

