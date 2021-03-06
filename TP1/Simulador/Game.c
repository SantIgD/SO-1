#include "Game.h"
#include <pthread.h>
#include "Barreras.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>


/******************************************************************************/
/* Representamos las células vivas como (0) y las muertas como (1) */
/******************************************************************************/

/******************************************************************************/
/* Implementación de la estructura _game */
/******************************************************************************/

struct _game{

    board_t* board;
    int ciclos;

};

/******************************************************************************/

/******************************************************************************/
/* Variables Globales */
/******************************************************************************/

barrier_t* barrera;
pthread_mutex_t lock;
int actualizando = 0,terminoCiclo = 0;
int indiceFila=0,indiceColumna=0;
int generacion=0,pausarEntre=0,mostrar=0;

/******************************************************************************/

/******************************************************************************/
/* Funciones internas */
/******************************************************************************/

/* Reinicializa las variables globales que se utilizan para el proceso de los ciclos y criterio divino*/
void reinicializar_globales();

/* Es el criterio de los dioses, busca la informacion y llevan a cabo los ciclos que definen las siguientes generaciones*/
void* criterio_divino(void* arg);

/* Obtiene la cantidad de vecinos vivos que tiene la celula en la posicion (row,col) del tablero*/
int get_vecinos_vivos(game_t* game,int row, int col);

/* Aplica las reglas para el proximo estado de la celula en cuestion*/
void aplicar_juicio(game_t* game, int row, int col, int sociedadViva);

/* Actualiza el tablero actual*/
void actualizar_tablero(game_t* game);

/* Condicional que establece cuando una celula vive/muere/se mantiene muerta/revive */
int mandato_vive(int vecinosVivos,int estadoActual);

/* Verifica que se pueda aplicar la funcion juicio_divino*/
int condicion_aplicar_juicio(game_t* game, int indiceFilaHilo,int indiceColumnaHilo);

/* Avanza los indices del tablero para que eventualmente se otorguen las tareas a los dioses*/
void avanzar_celula(game_t* game);

/* Chequea que los ciclos, filas y columnas sean correctos */
void first_line_checker(game_t * game,int filas,int columnas);

/* Chequea que ya se hayan actualizado todas las velulas del tablero*/
void chequear_fin_ciclo(game_t* game, int indiceFilaHilo,int indiceColumnaHilo);

/* Obtiene la cantidad de vecinos vivos y llama a aplicar juicio*/
void juicio_divino(game_t* game,int row, int col);

/* Implementacion de algoritmo de ciclo*/
void do_ciclo(game_t* game, int indiceFilaHilo, int indiceColumnaHilo);


/******************************************************************************/

/******************************************************************************/
/* Creacion, inicializacion y destruccion del juego*/
/******************************************************************************/

game_t * game_create(){
    
    // Asignamos memoria a la estructura _game.
    return malloc(sizeof(game_t));
  
}

int game_init(game_t* game, char* filename){
    
    game->board = board_create();

    game_load(game,filename);

    return 0;
  
}


int game_load(game_t* game, char *filename){

    int filas,columnas;
    FILE* archivo = fopen(filename,"r");

    // Verificamos que se haya podido abrir el archivo
    if(archivo == NULL){
        perror("No se pudo abrir el archivo! ");
        exit(EXIT_FAILURE);
    }

    // Cargamos la cantidad de ciclos, filas y columnas del archivo
    fscanf(archivo, "%d %d %d",&game->ciclos,&filas,&columnas);
    fclose(archivo);

    // Verificamos que la cantidad de ciclos, filas y columnas no sean negativas.
    first_line_checker(game,filas,columnas);

    board_cells_create(game->board,filas,columnas);

    board_init(game->board,filename);

    return 0;

}

void game_writeBoard(game_t* game,char *filename){

    int largo = strlen(filename);
    char* filenameSalida = malloc(sizeof(char) * (largo+2));
  
    strncpy(filenameSalida, filename, largo - 4);

    filenameSalida[largo - 4] = '\0'; // Borra hasta el punto sin incluir (para la extension .game)

    strcat(filenameSalida, "final");

    board_write(game->board,filenameSalida);

    free(filenameSalida);

}


void reinicializar_globales(){

    terminoCiclo  = 0;
    indiceFila    = 0;
    indiceColumna = 0;
    actualizando  = 0;

}


void game_destroy(game_t* game){

    // Liberamos la memoria de game
    board_destroy(game->board);
    free(game);
}

/******************************************************************************/

/******************************************************************************/
/* Jugando */
/******************************************************************************/

void* criterio_divino(void* arg){

    game_t* game = (game_t*) arg;

    int indiceFilaHilo ,indiceColumnaHilo;

    for(int i=0; i < game->ciclos; i++){

        do_ciclo(game,indiceFilaHilo,indiceColumnaHilo);
        
        barrier_wait(barrera);
    
        /* Actualizar tablero */ 

        pthread_mutex_lock(&lock);

        if(actualizando == 1){
            actualizar_tablero(game);
        }

        pthread_mutex_unlock(&lock); 
                    
        barrier_wait(barrera); // Evita que se actualice mas de 1 vez

    }

    pthread_exit(EXIT_SUCCESS);
}

/******************************************************************************/

/******************************************************************************/
/* Inicia el juego, creacion de hilos y asignacion de tareas */
/******************************************************************************/

int congwayGoL(game_t *game, const int nuproc){

    char resp;

    printf("Si desea mostrar generaciones ingrese S, sino cualquier letra: ");
    scanf(" %c",&resp);
    if(resp == 'S' || resp == 's'){
        mostrar = 1;

        printf("Si desea pausar entre generaciones ingrese S, sino cualquier letra: ");
        scanf(" %c",&resp);
        if(resp == 'S' || resp == 's'){
        pausarEntre = 1;
    }
    }

    game_show(game);
    /* futuros hilos */
    pthread_t dios[nuproc];

    /* Inicializacion candado y barrera*/
    pthread_mutex_init(&lock,NULL);
    barrera = barrier_create();
    barrier_init(barrera,nuproc);
    

    /* Creacion de hilos */
    for(int i=0; i < nuproc; i++){

        /* Habilitamos a los dioses */
        assert(!pthread_create( &dios[i]
                                , NULL
                                , criterio_divino
                                , (void*) game));
    
    }    
        
    /* Esperar a que terminen */
    for(int i=0; i < nuproc; i++)
        assert(! pthread_join(dios[i], NULL));

    /* Destruimos la barrera y el candado */
    barrier_destroy(barrera);
    pthread_mutex_destroy(&lock);

    return 0;
}

/******************************************************************************/

/******************************************************************************/
/* Informacion del juego */
/******************************************************************************/

int game_getCantFilas(game_t* game){

    return board_getCantFilas(game->board);
}

int game_getCantColumnas(game_t* game){

    return board_getCantColumnas(game->board);
}

void game_show(game_t* game){

    if (mostrar){
        printf("\n----------------------------\n  Tablero Generacion %d \n----------------------------\n\n",generacion);
        board_show(game->board);

        if (pausarEntre){
            //getchar();
            sleep(1);
        }
        printf("\n");
    }
    
}

int get_vecinos_vivos(game_t* game,int row, int col){

    int sociedadViva = 8;

    sociedadViva -= game_vecino_superior(game,row,col);
    sociedadViva -= game_vecino_superior_i(game,row,col);   
    sociedadViva -= game_vecino_superior_d(game,row,col);

    sociedadViva -= game_vecino_inferior(game,row,col);
    sociedadViva -= game_vecino_inferior_i(game,row,col);
    sociedadViva -= game_vecino_inferior_d(game,row,col);

    sociedadViva -= game_vecino_lateral_i(game,row,col);
    sociedadViva -= game_vecino_lateral_d(game,row,col);

    return sociedadViva;

}

int game_vecino_superior(game_t *game,int row,int col){

    if ( row == 0 ){

        row = game_getCantFilas(game);

    }

    int filaAnterior = (row-1) % game_getCantFilas(game);
    
    return board_get(game->board,filaAnterior,col);
}

int game_vecino_superior_i(game_t *game,int row,int col){

    if ( row == 0 ){

        row = game_getCantFilas(game);

    }

    int filaAnterior    = (row - 1) % game_getCantFilas(game);

    if ( col == 0 ){

        col = game_getCantColumnas(game);

    }
    int columnaAnterior = (col - 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaAnterior,columnaAnterior);
}

int game_vecino_superior_d(game_t *game,int row,int col){

    if ( row == 0 ){

        row = game_getCantFilas(game);

    }

    int filaAnterior     = (row - 1) % game_getCantFilas(game);
    int columnaPosterior = (col + 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaAnterior,columnaPosterior);
}

int game_vecino_inferior(game_t *game,int row,int col){

    int filaPosterior = (row + 1) % game_getCantFilas(game);
   
    return board_get(game->board,filaPosterior,col);
}

int game_vecino_inferior_i(game_t *game,int row,int col){

    int filaPosterior     = (row + 1) % game_getCantFilas(game);

    if ( col == 0 ){

        col = game_getCantColumnas(game);

    }
    
    int columnaAnterior   = (col - 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaPosterior,columnaAnterior);
}

int game_vecino_inferior_d(game_t *game,int row,int col){

    int filaPosterior     = (row + 1) % game_getCantFilas(game);
    int columnaPosterior  = (col + 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaPosterior,columnaPosterior);
}

int game_vecino_lateral_i(game_t *game,int row,int col){
    
    if ( col == 0 ){

        col = game_getCantColumnas(game);

    }
    int columnaAnterior  = (col - 1) % game_getCantColumnas(game);
    
    return board_get(game->board,row,columnaAnterior);
}

int game_vecino_lateral_d(game_t *game,int row,int col){

    int columnaPosterior  = (col + 1) % game_getCantColumnas(game);
    
    return board_get(game->board,row,columnaPosterior);
}
/******************************************************************************/

/******************************************************************************/
/* Modificar tablero */
/******************************************************************************/

void aplicar_juicio(game_t* game, int row, int col, int sociedadViva){

    int estadoActual = board_get(game->board,row,col);
    
    if (mandato_vive(sociedadViva,estadoActual) == ALIVE){

        board_proxGen_set(game->board,row,col,ALIVE);

    }else{

        board_proxGen_set(game->board,row,col,DEAD);
    }

}

void game_set_value(game_t* game, int row, int col, int value){
    board_proxGen_set(game->board,row,col,value);
}

void actualizar_tablero(game_t* game){


        board_interchange(game->board);

        generacion++;

        game_show(game);

        reinicializar_globales();

}
/******************************************************************************/

/******************************************************************************/
/* Condicionales */
/******************************************************************************/

int mandato_vive(int vecinosVivos,int estadoActual){

    int resp = DEAD;


    if ((estadoActual == ALIVE && (vecinosVivos == 2 || vecinosVivos ==3))
                            || 
        (estadoActual == DEAD && vecinosVivos == 3)){
            resp = ALIVE;
        }
    return resp;

}

int condicion_aplicar_juicio(game_t* game, int indiceFilaHilo,int indiceColumnaHilo){

    return (!terminoCiclo 
            || (indiceFilaHilo == game_getCantFilas(game)-1
            &&  indiceColumnaHilo == game_getCantColumnas(game)-1));
}

/******************************************************************************/

/******************************************************************************/
/* Mecanismos internos */
/******************************************************************************/

void avanzar_celula(game_t* game){
    
    indiceColumna++;
    if(indiceColumna == game_getCantColumnas(game)){
        indiceColumna = 0;
        indiceFila++;
    }
}

void first_line_checker(game_t* game, int filas, int columnas){
    
    if (game->ciclos < 1){

        perror("La cantidad de Ciclos no es valido. ");
        exit(EXIT_FAILURE);
        
    }else if (filas < 1){
        perror("La cantidad de Filas no es valida. ");
        exit(EXIT_FAILURE);

    }else if (columnas < 1){
        perror("La cantidad de Columnas no es valida. ");
        exit(EXIT_FAILURE);
    }
}

void chequear_fin_ciclo(game_t* game, int indiceFilaHilo,int indiceColumnaHilo){

    int ultimaFila = game_getCantFilas(game)-1 
       ,ultimaColumna = game_getCantColumnas(game)-1;

    if (indiceFilaHilo == ultimaFila 
        && indiceColumnaHilo == ultimaColumna){
        
        terminoCiclo = 1;
        actualizando = 1;
    }
}

void juicio_divino(game_t* game,int row, int col){

    int sociedadViva = get_vecinos_vivos(game,row,col);

    aplicar_juicio(game,row,col,sociedadViva);

}

void do_ciclo(game_t* game, int indiceFilaHilo, int indiceColumnaHilo){

    while(!terminoCiclo){

        pthread_mutex_lock(&lock);

        if (!terminoCiclo){
            
            indiceFilaHilo = indiceFila;
            indiceColumnaHilo = indiceColumna;

            avanzar_celula(game);

            chequear_fin_ciclo(game,indiceFilaHilo,indiceColumnaHilo);
        }

        pthread_mutex_unlock(&lock);

        
        if (condicion_aplicar_juicio(game,indiceFilaHilo,indiceColumnaHilo)){

            juicio_divino(game,indiceFilaHilo,indiceColumnaHilo);
        }
        

    }

    
}

/******************************************************************************/

