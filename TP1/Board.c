#include "Board.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


struct _board{

    int** tableroActual;
    int** tableroProxGen;
    int cantColumnas;
    int cantFilas;
};

/* Funciones Internas */
int get_state(char val);


/* Creacion, inicializacion y destruccion del tablero */

board_t* board_create(){
    return malloc(sizeof(board_t));
}

int board_cells_create(board_t* board,int row,int col){

    board->cantColumnas = col;
    board->cantFilas = row;

    board->tableroActual = malloc(sizeof(int*) * board->cantFilas );
    board->tableroProxGen = malloc(sizeof(int*) * board->cantFilas );

    for (int i = 0; i < board->cantFilas; i++){
        board->tableroActual[i] = malloc(sizeof(int) * board->cantColumnas );
        board->tableroProxGen[i] = malloc(sizeof(int) * board->cantColumnas );
    }


    return 0;
}

int board_load(board_t *board, char *str,int row){
    
    int columna=0 , cantidad=0;
    int longitud = strlen(str);
    
    for(int i = 0; i < longitud; i=i+2){

        cantidad = atoi((const char* ) &str[i]);
        board_actual_set(board,row,columna,cantidad,str[i+1]);
        columna += cantidad;

    }

    
    if (columna != board->cantColumnas){

        perror("No se ingreso el numero de columnas correctas ");
        exit(EXIT_FAILURE);
    }

    return 0;
}

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

void board_destroy(board_t *board){

    

    for (int i = 0; i < board->cantFilas; i++){
        free(board->tableroActual[i]);
        free(board->tableroProxGen[i]);
    }
    free(board->tableroActual);
    free(board->tableroProxGen);

    free(board);
}

/* Internas */

int get_state(char val){

    int estado = 0;

    if (val=='X'){
        estado = 1;

    }
    
    return estado;
}

/* Setear informacion del tablero */

int board_actual_set(board_t* board,int row,int col,int cant,char val){

    int estado = get_state(val);

    for(int j = 0; j < cant; j++){
        
        board->tableroActual[row][col+j]=estado;
    }
   
    return 0;
}

int board_proxGen_set(board_t* board,int row,int col,int valor){
    
    board->tableroProxGen[row][col]=valor;   
    return 0;
}

void board_interchange(board_t* board){
    
    int** aux = board->tableroProxGen;
    board->tableroProxGen = board->tableroActual;
    board->tableroActual = aux;

}

/* Obtener informacion del tablero*/
int board_get(board_t* board, unsigned int row, unsigned int col){
    return board->tableroActual[row][col];
}

int board_getCantFilas(board_t * board){
    return board->cantFilas;
}

void board_show(board_t* board){

    for(int i =0; i < board->cantFilas; i++){

        printf("   ");

        for(int j = 0; j < board->cantColumnas; j++){

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

int board_getCantColumnas(board_t * board){
    return board->cantColumnas;
}
