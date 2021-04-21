#include "Board.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/******************************************************************************/
/* Implementación de la estructura _board */
/******************************************************************************/

struct _board{

    int** tableroActual;
    int** tableroProxGen;
    int cantColumnas;
    int cantFilas;
};

/******************************************************************************/

/******************************************************************************/
/* Funciones Internas */
/******************************************************************************/

/* Indica si una celula esta viva o no devolviendo 0 o 1 respectivamente*/
int get_state(char val);

/******************************************************************************/

/******************************************************************************/
/* Creacion, inicializacion y destruccion del tablero */
/******************************************************************************/

board_t* board_create(){

    // Asignamos memoria a la estructura _board.
    return malloc(sizeof(board_t));

}

int board_cells_create(board_t* board,int row,int col){

    // Establecemos la cantidad de columnas y filas 
    board->cantColumnas = col;
    board->cantFilas = row;

    // Asignamos memoria a tableroActual y tableroProxGen de board
    board->tableroActual = malloc(sizeof(int*) * board->cantFilas );
    board->tableroProxGen = malloc(sizeof(int*) * board->cantFilas );

    for (int i = 0; i < board->cantFilas; i++){
        board->tableroActual[i] = malloc(sizeof(int) * board->cantColumnas );
        board->tableroProxGen[i] = malloc(sizeof(int) * board->cantColumnas );
    }

    return 0;

}

int obtener_cantidad(char* str,int indice,int cant_indiceChar[2]){
    char valor[100];
    int i=0;
    
    for (; str[indice] != '\n' && str[indice] != 'X' && str[indice] != 'O'; indice++ ){
        valor[i] = str[indice];
        i++;
    }
    valor[i] = '\0';

    cant_indiceChar[0] = atoi(valor);
    cant_indiceChar[1] = strlen(valor);

    return 0;
    

}

int board_load(board_t *board, char *str,int row){
    
    int columna=0 , cantidad=0;
    int longitud = strlen(str);
    int cant_indiceChar[2];
    
    //printf("longitud :%d",longitud);
    // Cargamos los datos en la fila row del tableroActual 
    // de board desde str
    for(int i = 0; i < longitud-1;){

        obtener_cantidad(str,i,cant_indiceChar);
        board_actual_set(board,row,columna,cant_indiceChar[0],str[cant_indiceChar[1]+i]);

        columna += cant_indiceChar[0];
        i+=cant_indiceChar[1]+1;


    }

    // Verificamos que la cantidad de columnas sea correcta
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
    char finalLinea;

    FILE* archivo = fopen(filename,"r");

    // Verificamos que se haya podido abrir el archivo
    if(archivo == NULL){

        perror("No se pudo abrir el archivo! ");
        exit(EXIT_FAILURE);
    }

    // Perdemos la primera linea, que ya la leimos 
    // (con 1024 caracteres leidos nos alcanza para que lea el \n de fin de linea)   
    fgets(linea,1024,archivo);

    // Linea 0 del tableroActual de board
    fgets(linea,cantidadLetras,archivo); 

    // Cargamos el tableroActual de board
    while(!feof(archivo)){ 
        
        if (fila < board->cantFilas){
            board_load(board,linea,fila);
        }
        
        fila++;
        fgets(linea,cantidadLetras,archivo); 
    }

    
    // Verificamos que la cantidad de columnas sea correcta
    if (fila != board->cantFilas){

        perror("La semilla ingresada es incorrecta por cantidad de filas");
        exit(EXIT_FAILURE);
    }
        

    fclose(archivo);
    return 0;

}

void board_write(board_t* board, char* filename){

    FILE* archivo = fopen(filename,"w");

    // Guardamos el tableroActual en el archivo
    // cambiando los 1´s y 0´s por X´s y O´s respectivamente
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

    // Liberamos la memoria de board
    for (int i = 0; i < board->cantFilas; i++){
        free(board->tableroActual[i]);
        free(board->tableroProxGen[i]);
    }
    free(board->tableroActual);
    free(board->tableroProxGen);

    free(board);

}

/******************************************************************************/


/******************************************************************************/
/* Setear informacion del tablero */
/******************************************************************************/

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

/******************************************************************************/

/******************************************************************************/
/* Informacion del tablero*/
/******************************************************************************/

int board_get(board_t* board, int row, int col){

    return board->tableroActual[row][col];
}

int board_getCantFilas(board_t * board){

    return board->cantFilas;
}

int board_getCantColumnas(board_t * board){

    return board->cantColumnas;
}

void board_show(board_t* board){

    // Imprimimos el tableroActual en pantalla
    // cambiando los 1´s y 0´s por X´s y O´s respectivamente
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

/******************************************************************************/

/******************************************************************************/
/* Mecanismos Internos */
/******************************************************************************/

int get_state(char val){

    int estado = 0;

    if (val=='X'){
        estado = 1;

    }
    
    return estado;

}

/******************************************************************************/