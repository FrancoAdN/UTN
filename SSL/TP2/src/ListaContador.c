#include "Comun.h"

struct ListaContador {
    char* valor;
    int contador;
    struct ListaContador* siguiente;
};

struct ListaContador* ListaIdentificadores = NULL;
struct ListaContador* ListaOperadores = NULL;

/**
* Crea la estructura en la que se almacenan los identificadores u operadores
* que fueron reconocidas por el analizador lexico.
* Si el identificador u operador ya existe en la lista, se le suma a su contador
* @param {char*}                  yytext - Valor de la literal cadena reconocida
* @param {struct ListaContador**} Lista  - Lista en la cual insertar el nuevo valor
*/
void crearEstructuraListaContador(char* yytext, struct ListaContador** Lista){
    struct ListaContador* actual = *Lista;
    struct ListaContador* previo = NULL;
    // Busca el identificador en la lista
    while (actual != NULL && strcmp(actual->valor, yytext) < 0) {
        previo = actual;
        actual = actual->siguiente;
    }

    // Si el identificador ya está en la lista, aumenta su contador
    if (actual != NULL && strcmp(actual->valor, yytext) == 0) {
        actual->contador++;
    } else {
        // Crea un nuevo nodo para el identificador
        struct ListaContador* nuevo = (struct ListaContador*)malloc(sizeof(struct ListaContador));
        nuevo->valor = strdup(yytext);
        nuevo->contador = 1;
        nuevo->siguiente = actual;

        // Actualiza el enlace anterior si existe, de lo contrario, actualiza el inicio de la lista
        if (previo != NULL) {
            previo->siguiente = nuevo;
        } else {
            *Lista = nuevo;
        }
    }

}

/**
* Imprime en un fichero de salida el reporte de los identificadores encontrados
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirListaIdentificadores(FILE* salida) {
    fprintf(salida, "%sListado de Identificadores\n\n", generarEspacios(8));
    struct ListaContador* actual = ListaIdentificadores;
    while (actual != NULL) {
        fprintf(salida, "Identificador: %s - Cantidad: %i\n", actual->valor, actual->contador);
        struct ListaContador* aux = actual;
        actual = actual->siguiente;
        free(aux->valor);
        free(aux);
    }
}

/**
* Imprime en un fichero de salida el reporte de los operadores encontrados
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirListaOperadores(FILE* salida) {
    fprintf(salida, "%sListado de Operadores\n\n", generarEspacios(8));
    struct ListaContador* actual = ListaOperadores;
    while (actual != NULL) {
        fprintf(salida, "Operador: %s - Cantidad: %i\n", actual->valor, actual->contador);
        struct ListaContador* aux = actual;
        actual = actual->siguiente;
        free(aux->valor);
        free(aux);
    }
}