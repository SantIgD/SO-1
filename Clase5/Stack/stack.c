#include "stack.h"
#include <pthread.h>
#include <stdlib.h>

#define K 10

pthread_mutex_t candado = PTHREAD_MUTEX_INITIALIZER;

struct stack_t{

    int* elementos;
    int cabezal;
    
};



stack stack_init(){
    
    stack s = malloc(sizeof(stack));
    s->elementos=malloc(sizeof(int) * K);
    s->cabezal=0;
    return s;
}

void destroy(stack s){
    free(s->elementos);
    free(s);
}


void stack_push(stack s,int elemento){

    pthread_mutex_lock(&candado);

    s->elementos[s->cabezal] = elemento;

    s->cabezal= s->cabezal + 1;
    
    pthread_mutex_unlock(&candado);
    
}

int stack_top(stack s){

    pthread_mutex_lock(&candado);

    int ret = s->elementos[0];
    
    pthread_mutex_unlock(&candado);
    return ret;
}

void stack_pop(stack s){

    pthread_mutex_lock(&candado);
    for(int i=0;i<=s->cabezal-1;i++){

        s->elementos[i] = s->elementos[i+1]; ;
    }
    s->cabezal= s->cabezal - 1;
    
    pthread_mutex_unlock(&candado);
    
}