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

void osiris(game_t* game){
    
    indiceColumna++;
    if(indiceColumna == game_getCantColumnas(game)){
        indiceColumna = 0;
        indiceFila++;
    }
}

void* criterio_divino(void* arg){

    game_t* game = (game_t*) arg;

    int indiceFilaHilo ,indiceColumnaHilo;
    for(int i=0; i < game->ciclos; i++){

        while(!terminoCiclo){

            pthread_mutex_lock(&lock);

            if (!terminoCiclo){
                osiris(game);
            

                indiceFilaHilo = indiceFila;
                indiceColumnaHilo = indiceColumna;

                if (indiceFilaHilo == game_getCantFilas(game)-1
                &&  indiceColumnaHilo == game_getCantColumnas(game)-1){
                    terminoCiclo = 1;
                    actualizando = 1;
                }
            }

            pthread_mutex_unlock(&lock);

            
            if (!terminoCiclo || 
            indiceFilaHilo == game_getCantFilas(game)-1
            &&  indiceColumnaHilo == game_getCantColumnas(game)-1){
                pri
                ntf("Coordenadas en el tablero (%d,%d)\n ",indiceFilaHilo,indiceColumnaHilo);
                //aplicar_juicio(indiceFilaHilo,indiceColumnaHilo);
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