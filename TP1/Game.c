#include "Game.h"
#include <pthread.h>
#include "Barreras.h"
#include <stdio.h>
#include <assert.h>


/******************************************************************************/
/* Representamos las células vivas como 'O' y las muertas como 'X' */
/******************************************************************************/
struct _game{

    board_t* board;
    int ciclos;

};

barrier_t* barrera;
pthread_mutex_t lock;
int actualizando = 1,terminoCiclo = 0;
int indiceFila=0,indiceColumna=0;

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
void game_writeBoard(game_t* game,char *filename){

    board_write(game->board,filename);

}

int game_getCantFilas(game_t* game){

    return board_getCantFilas(game->board);
}

int game_getCantColumnas(game_t* game){

    return board_getCantColumnas(game->board);
}

void avanzar_celula(game_t* game){
    
    indiceColumna++;
    if(indiceColumna == game_getCantColumnas(game)){
        indiceColumna = 0;
        indiceFila++;
    }
}

int vecino_superior(game_t *game,int row,int col){

    if ( row == 0 ){

        row = game_getCantFilas(game);

    }

    int filaAnterior = (row-1) % game_getCantFilas(game);
    
    return board_get(game->board,filaAnterior,col);
}

int  vecino_superior_i(game_t *game,int row,int col){

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

int vecino_superior_d(game_t *game,int row,int col){

    if ( row == 0 ){

        row = game_getCantFilas(game);

    }

    int filaAnterior     = (row - 1) % game_getCantFilas(game);
    int columnaPosterior = (col + 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaAnterior,columnaPosterior);
}

int  vecino_inferior(game_t *game,int row,int col){

    int filaPosterior = (row + 1) % game_getCantFilas(game);
   
    return board_get(game->board,filaPosterior,col);
}

int  vecino_inferior_i(game_t *game,int row,int col){

    int filaPosterior     = (row + 1) % game_getCantFilas(game);

    if ( col == 0 ){

        col = game_getCantColumnas(game);

    }
    
    int columnaAnterior   = (col - 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaPosterior,columnaAnterior);
}

int vecino_inferior_d(game_t *game,int row,int col){

    int filaPosterior     = (row + 1) % game_getCantFilas(game);
    int columnaPosterior  = (col + 1) % game_getCantColumnas(game);
    
    return board_get(game->board,filaPosterior,columnaPosterior);
}

int vecino_lateral_i(game_t *game,int row,int col){
    
    if ( col == 0 ){

        col = game_getCantColumnas(game);

    }
    int columnaAnterior  = (col - 1) % game_getCantColumnas(game);
    
    return board_get(game->board,row,columnaAnterior);
}

int vecino_lateral_d(game_t *game,int row,int col){

    int columnaPosterior  = (col + 1) % game_getCantColumnas(game);
    
    return board_get(game->board,row,columnaPosterior);
}


void game_set_value(game_t* game, int row, int col, int value){
    board_proxGen_set(game->board,row,col,value);
}

int mandato_vive(int vecinosVivos,int estadoActual){

    int resp = DEAD;

    if ((estadoActual == ALIVE && (vecinosVivos == 2 || vecinosVivos ==3))
                            || 
        (estadoActual == DEAD && vecinosVivos == 3)){
            resp = ALIVE;
        }
    return resp;

}

void juicio_divino(game_t* game,int row, int col){

    int sociedadViva = 8;

    sociedadViva -= vecino_superior(game,row,col);
    sociedadViva -= vecino_superior_i(game,row,col);   
    sociedadViva -= vecino_superior_d(game,row,col);

    sociedadViva -= vecino_inferior(game,row,col);
    sociedadViva -= vecino_inferior_i(game,row,col);
    sociedadViva -= vecino_inferior_d(game,row,col);

    sociedadViva -= vecino_lateral_i(game,row,col);
    sociedadViva -= vecino_lateral_d(game,row,col);

    int estadoActual = board_get(game->board,row,col);

    
    
    if (mandato_vive(sociedadViva,estadoActual) == ALIVE){

        //printf("[VIVE] (%d,%d) Estado actual = %d, vivos = %d\n",row,col,estadoActual,sociedadViva);    
        board_proxGen_set(game->board,row,col,ALIVE);

    }else{

        //printf("[MUERE] (%d,%d) Estado actual = %d, vivos = %d\n",row,col,estadoActual,sociedadViva);
        board_proxGen_set(game->board,row,col,DEAD);
    }


}

void* criterio_divino(void* arg){

    game_t* game = (game_t*) arg;

    int indiceFilaHilo ,indiceColumnaHilo;
    for(int i=0; i < game->ciclos; i++){

        while(!terminoCiclo){

            pthread_mutex_lock(&lock);

            if (!terminoCiclo){
                
                indiceFilaHilo = indiceFila;
                indiceColumnaHilo = indiceColumna;

                avanzar_celula(game);

                if (indiceFilaHilo == game_getCantFilas(game)-1
                &&  indiceColumnaHilo == game_getCantColumnas(game)-1){
                    terminoCiclo = 1;
                    actualizando = 1;
                }
            }

            pthread_mutex_unlock(&lock);

            
            if (!terminoCiclo 
               || (indiceFilaHilo == game_getCantFilas(game)-1
               &&  indiceColumnaHilo == game_getCantColumnas(game)-1)){
                //printf("Coordenadas en el tablero (%d,%d)\n ",indiceFilaHilo,indiceColumnaHilo);
                juicio_divino(game,indiceFilaHilo,indiceColumnaHilo);
            }
            

        }

        

            //barrier
            barrier_wait(barrera);
          

            //---> actualizamos
            pthread_mutex_lock(&lock);
            if(actualizando == 1){

                board_interchange(game->board);
                game_show(game);
                terminoCiclo  = 0;
                indiceFila    = 0;
                indiceColumna = 0;
                actualizando  = 0;
            }
            pthread_mutex_unlock(&lock);
                
            //barrier
            barrier_wait(barrera);
        
        //printf("vamos por la iteracion: %d\n", i );

    }

    

    pthread_exit(EXIT_SUCCESS);
}

/* Simulamos el Juego de la Vida de Conway con tablero 'board' la cantidad de
ciclos indicados en 'cycles' en 'nuprocs' unidades de procesamiento*/
int congwayGoL(game_t *game, const int nuproc){

    pthread_t dios[nuproc];
    pthread_mutex_init(&lock,NULL);
    barrera = barrier_create();
    barrier_init(barrera,nuproc);
    


        //crear hilos
    for(int i=0; i < nuproc; i++){

        /* Habilitamos a los dioses */
        assert(!pthread_create( &dios[i]
                                , NULL
                                , criterio_divino
                                , (void*) game));
    
    }    
        
    //cerrarlos
    for(int i=0; i < nuproc; i++)
        assert(! pthread_join(dios[i], NULL));

}
 

void game_show(game_t* game){

    printf("\n------------------\n  Tablero actual \n------------------\n\n");
    board_show(game->board);
    printf("\n");
}

void game_destroy(game_t* game){

    board_destroy(game->board);
    free(game);
}