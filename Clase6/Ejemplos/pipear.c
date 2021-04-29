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
    

    if (argc == 3){

        char *args[] = {"./HelloPid", NULL};
        int state;
        
            
        assert(! pipe(fd));
        assert((p = fork()) >= 0);
        
        switch (p){
            
            case  CHILD:{
                
                /* Aqui prog1*/

                execv(argv[1],args);
                break;
            }

            default:{ /* Parent */

                /* Aqui prog2*/
                wait(&state);

                dup2(0,1);
                execv(argv[2],args);

                break;
            }                                    
        

        }
        ret = 0;
    }
    
    return ret;
}