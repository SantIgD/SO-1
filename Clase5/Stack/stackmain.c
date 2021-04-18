#include "stack.h"
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>


int main(){

    stack s=stack_init();

    for(int i=0;i<10;i++){
        stack_push(s,i);
        printf("Se pusheo %d ---- top = %d\n",i,stack_top(s));
    }

    for(int i=0;i<10;i++){
        
        printf("Se va a popear, top = %d\n",stack_top(s));
        stack_pop(s);
    }
    


    return 0;
}

