#Este es el Makefile para 
#compilar el programa simulador.c corectamente
simulador: Board.o Game.o 
	gcc -o simulador simulador.c Game.o Board.o Barreras.o -lpthread

Board.o: Board.c Board.h
	gcc -c Board.c

Barreras.o: Barreras.c Barreras.h
	gcc -c Barreras.c

Game.o: Game.c Game.h
	gcc -c Game.c

#Regla para eliminar el archivos creados por la compilación
clean:
	rm -f simulador *.o