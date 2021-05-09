#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <assert.h>
#include <wait.h>
#include <string.h>

#define CHILD 0

int main(int argc, char* argv[]){
    int ret = -1;
    int p;
    int fd[2];
    

    if (3 == 3){

        assert(! pipe(fd));
        char *args1[] = {"./escritura_s", NULL};
        char *args2[] = {"./lectura_s", NULL};
        int state;
        
            // /proc/pid/fd
        
        assert((p = fork()) >= 0);
        
        switch (p){
            
            case  CHILD:{
                close(fd[0]);
                /* Aqui prog1*/

                dup2(fd[1],STDOUT_FILENO);
                /*
                    old fd[0]
                    new STDOUT_FILENO 
                    old<-new
                
                */
                execv(args1[0],args1);
                break;
            }

            default:{ /* Parent */
                close(fd[1]);
                /* Aqui prog2*/
                //wait(&state);

                
                dup2(fd[0],STDIN_FILENO);
                execv(args2[0],args2);

                break;
            }                                    
        

        }
        ret = 0;
    }
    
    return ret;
}