#include "Comun.h"

struct CaracterNoReconocido {
    char* valor;
    int linea;
    struct CaracterNoReconocido* siguiente;
};

struct CaracterNoReconocido* ListaNoReconocido = NULL;

/**
* Crea la estructura en la que se almacenan los tokens no reconocidos
* por el analizador lexico
* @param {char*} yytext - Valor de la literal cadena reconocida
* @param {int} yylineno - Linea en la que se encuentra el token
*/
void crearEstructuraNoReconocido(char* yytext, int yylineno) {
    struct CaracterNoReconocido* nuevo = (struct CaracterNoReconocido*)malloc(sizeof(struct CaracterNoReconocido));
    nuevo->valor = strdup(yytext);
    nuevo->linea = yylineno;
    nuevo->siguiente = ListaNoReconocido;
    ListaNoReconocido = nuevo;
}

/**
* Imprime en un fichero de salida el reporte de los tokens no reconocidos encontrados
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirCaracteresNoReconocidos(FILE* salida) {
    fprintf(salida, "%sListado de Tokens no reconocidos\n\n", generarEspacios(8));
    struct CaracterNoReconocido* actual = ListaNoReconocido;
    while (actual != NULL) {
        fprintf(salida, "Token no reconocido: %s\t en la linea: %i\n", actual->valor, actual->linea);
        struct CaracterNoReconocido* aux = actual;
        actual = actual->siguiente;
        free(aux->valor);
        free(aux);
    }
}