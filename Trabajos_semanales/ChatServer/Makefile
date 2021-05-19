
#Este es el Makefile para la compilacion del servidor y el cliente.
servidor: RemoteChatServer.c
	gcc -o servidor RemoteChatServer.c -lpthread
cliente: RemoteChatClient.c
	gcc -o cliente RemoteChatClient.c -lpthread


#Regla para eliminar el archivos creados por la compilación
clean:
	rm -f servidor cliente *.o
   
