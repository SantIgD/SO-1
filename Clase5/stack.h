#ifndef __STACK_H__

#define __STACK_H__

struct stack_t;
typedef struct stack_t* stack;

stack stack_init();

void destroy(stack s);

void stack_push(stack s,int elemento);

int stack_top(stack s);

void stack_pop(stack s);


#endif