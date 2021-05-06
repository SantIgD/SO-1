#ifndef CREAR_DGRAM

#define CREAR_DGRAM

int crear_socket_nombrado(const char * archivo);
int rm_socket_nombrado(const char * archivo, int socket);
int terminar_programa(const char * archivo, int socket);
#endif
