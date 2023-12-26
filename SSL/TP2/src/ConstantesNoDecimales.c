#include "Comun.c"

struct ConstantesEnteras { 
    char* constante;
    int decimal;
    struct ConstantesEnteras* siguiente;
};

/* Inicializacion de listas */

struct ConstantesEnteras* ListaConstantesOctales = NULL;
struct ConstantesEnteras* ListaConstantesHexadecimales = NULL;

/* Fin inicializacion de listas */

/**
* Crea la estructura en la que se almacenan las constantes enternas no decimales
* que fueron reconocidas por el analizador lexico
* @param {char*}                      yytext - Valor de la literal cadena reconocida
* @param {struct ConstantesEnteras**} Lista  - Lista en la cual agregar el nuevo valor
* @param {short}                      base   - Base en la cual se encuentra la constante
*/
void crearEstructuraConstante(char* yytext, struct ConstantesEnteras** Lista, short base) {
    struct ConstantesEnteras* nuevaConstante = 
        (struct ConstantesEnteras*)malloc(sizeof(struct ConstantesEnteras));
    nuevaConstante->constante = strdup(yytext);
    nuevaConstante->decimal = baseADecimal(yytext, base);
    nuevaConstante->siguiente = *Lista;
    *Lista = nuevaConstante;
}

/**
* Imprime en un fichero de salida el reporte de los comentarios encontrados
* @param {struct ListaBasica**} Lista  - Lista a imprimir
* @param {int}                  base   - Base numerica de la lista
* @param {FILE*}                salida - Fichero en el cual escribir
*/
void imprimirConstantes(struct ConstantesEnteras** Lista, short base, FILE* salida){
    
    fprintf(salida, "%sListado de Constantes %s\n\n", generarEspacios(8), baseAPalabra(base));
    struct ConstantesEnteras* actual = *Lista;
    while (actual != NULL) {
        fprintf(salida,"Constante %s: %s - Valor decimal de la constante: %d\n", baseAPalabra(base), actual->constante, actual->decimal);
        struct ConstantesEnteras* aux = actual;
        actual = actual->siguiente;
        free(aux->constante);
        free(aux);
    }
}
