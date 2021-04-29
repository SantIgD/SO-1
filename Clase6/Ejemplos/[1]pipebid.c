#include <stdio.h> /* Print */
#include <unistd.h> /* Read/Write + Fork */
#include <assert.h>
#include <stdlib.h>
#include "pipeBidireccional.h"

#define BUFFER 1024

int main(){
  int p;

  pipe_b* pip = pipe_b_create();

  pipe_b_init(pip);


  /* Fork */
  assert((p = fork()) >= 0);


  if (p == 0){ /* Child */
    
    pipe_b_close_begin_child(pip);
    pipe_b_parent_close_end(pip);
    
    pipe_b_child_write(pip,"Hi Father!");

    pipe_b_child_read(pip);

    
    pipe_b_close_begin_parent(pip);
    pipe_b_child_close_end(pip);
    
    pipe_b_destroy(pip);

    exit(EXIT_SUCCESS);

  } else { /* Parent */
    
    
    pipe_b_close_begin_parent(pip);
    pipe_b_child_close_end(pip);

    pipe_b_parent_write(pip,"Hi son! How are u?");

    pipe_b_parent_read(pip);

    
    pipe_b_close_begin_child(pip);
    pipe_b_parent_close_end(pip);
    
    pipe_b_destroy(pip);


    exit(EXIT_SUCCESS);
  }

  
  /* Código muerto */
  exit(EXIT_SUCCESS);
}
