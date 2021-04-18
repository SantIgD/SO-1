#ifndef __FILTRO_H__
#define __FILTRO_H__


struct filtro;
typedef struct filtro filtro_t;

/* Crea un filtro para _n_ hilos */
filtro_t* filtro(unsigned int n);

/* El hilo _id_ está intentando tomar el lock */
void filtro_lock(filtro_t* filtro, int id);

/* El hilo _id_ libera el lock */
void filtro_unlock(filtro_t* filtro, int id);

/* Destruye el hilo/libera memoria*/
void filtro_delete(filtro_t* filtro);

#endif/* __FILTRO__ */
