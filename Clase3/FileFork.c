#include <unistd.h> /* Write/Read/lseeki/fork */
#include <stdio.h> /* prints */
#include <fcntl.h> /* open/close */
#include <stdlib.h> /* STD error output*/

char lseek_read(int fd, off_t pos){
        if (fd < 0 ){
                perror("Lectura erronea");
                exit(EXIT_FAILURE);
        } 
        {
                int res;
                off_t p;
                char resC;

                /* Movemos el cabezal */
                p = lseek(fd, pos, SEEK_SET);
                if ( p < 0 ){
                          perror("Lectura erronea");
                        exit(EXIT_FAILURE);
                }

                /* Leemos */
                res = read(fd, &resC, 1);
                if (res <= 0){ 
                        perror("Lectura erronea");
                        exit(EXIT_FAILURE);
                }

                return resC;
        }
}

void lseek_write(int fd, int pos, const char c){
        if (fd < 0 ){
                perror("Escritura erronea");
                exit(EXIT_FAILURE);
        }
        int res;
        off_t p;

        /* Movemos el cabezal */
        p = lseek(fd, pos, SEEK_SET);
        if ( p < 0 ){
                perror("Escritura erronea");
                exit(EXIT_FAILURE);
        }

        /* Escribir */
        res = write(fd, &c, 1);
        if (res <= 0){ 
                perror("Escritura erronea");
                exit(EXIT_FAILURE);
        }

        return;
}

int main(int argc, char **argv){
  int fd;
  pid_t forkOut;


  /* Abrimos el archivo en modo de Escritura/Lectura */
  fd = open("./file", O_RDWR);

  /* Chequeamos que la apertura del archivo fue correcta*/
  if( fd < 0){
          perror("Error en la lectura");
          exit(EXIT_FAILURE);
  } 

  /* Hacemos un fork, creando un child*/
  forkOut = fork();

  if (forkOut < 0){
          perror("Creación del proceso");
          exit(EXIT_FAILURE);
  } 

  if(forkOut == 0){
          /* Child */
          char c;
          c = lseek_read(fd, 13);
          if(c == 'a'){
                  printf("[C]: leyó una a\n");
                  lseek_write(fd, 13, 'b');
                  printf("[C]: escribió una b\n");
          } else {
                  printf("[C]: leyó una %c\n", c);
                  lseek_write(fd, 13, 'z');
                  printf("[C]: escribió una z\n");
          }

	  /* Cerramos el FD */
	  close(fd);
          _exit(EXIT_SUCCESS);
  } else {
          char c;
          /* Parent */
          c = lseek_read(fd, 13);
          printf("[P]: leyó una %c\n",c);
          lseek_write(fd, 13, 'x');
          printf("[P]: escribió una x\n");

	  /* Cerramos el FD */
	  close(fd);
  }
  return 0;
}
