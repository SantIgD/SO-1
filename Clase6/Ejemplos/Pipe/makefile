FLAGS = -Wall -Wextra -Werror  -g
EJECUTABLE = programa

#-Wall y -Wextra: activan todos las advertencias
#-Werror: convierte las advertencias en errores
#-std=c99: usa el est´andar C99
#-g: genera informaci´on para el debugging

#-c: Compila el codigo sin linkear, se crea un archivo objeto.
#-o: cambia el nombre por defecto del archivo generado por uno elegido.
#-lm: para enlazar la libreria math
#-pthread: POSIX

compilar : escritura_s.c 
	gcc escritura_s.c -o escritura_s
	gcc lectura_s.c -o lectura_s
	gcc pipear.c -o pipear

ejecutar : compilar
	./pipear