# Filtro

La idea del algoritmo de filtro es la generalización del algoritmo de Peterson
para escribir *locks* para una cantidad `N>2` (determinada) de hilos.
Consiste en crear `N-1` salas de esperas, llamadas niveles, las cuales un hilo
tendrá que transcurrir para poder acceder a la región crítica.

Los niveles deben cumplir dos propiedades:

+ A lo sumo un hilo tratando de acceder al nivel `l` lo logra
+ Si hay más de un hilo tratando de acceder al nivel `l`, al menos un hilo se
  queda esperando en el nivel `l`

  [0] ---- [n-2] - [Ejecutar hilo]

  [0]---[1]----[n-2]
  
El algoritmo de Peterson utiliza un arreglo de dos elementos para indicar cuando
un hilo está intentado acceder a la sección crítica.
Se generaliza dicha idea a un arreglo `nivel` de `N` elementos, donde `nivel[j]`
indica el nivel más alto que el hilo `j` está tratando de acceder.
Además cada nivel `l` tiene una (distinta) *víctima* `vic[l]` que prohíbe a
dicho hilo de entrar al nivel `l`, a menos que no hay ningún hilo en el nivel
`l` o en cualquiera de los superiores.

indices de nivel -> hilos.
nivel[hilo] -> sala a la que quiere acceder

indices de vic -> salas
vic[sala] -> hilo que prohibe la entrada a esa sala

El ejercicio consiste en programar dicho algoritmo, pueden utilizar las
siguientes interfaces como guía:
```C
#filtro.h
typedef unsigned int** filtro_t;

/* Crea un filtro para _n_ hilos */
filtro_t filtro(unsigned int n**);

/* El hilo _id_ está intentando tomar el lock */
void filtro_lock(filtro_t filtro, int id);

/* El hilo _id_ libera el lock */
void filtro_unlock(filtro_t filtro, int id);


       
      nivel = [ [-1] [-1] [-1] [-1] [-1] [-1] ]
      vic   = [ [-1] [-1] [-1] [-1] [-1] [-1] ]

      sala 0 => ninguna sala

      hilo -1 => ningun hilo esta prohibiendo


      nivelInteresado[hilo] -> sala

      patovaSala[sala] -> hilo

      

