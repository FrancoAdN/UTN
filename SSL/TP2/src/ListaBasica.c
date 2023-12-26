#include "Comun.h"

struct ListaBasica{
    char* constante;
    struct ListaBasica* siguiente;
};

/* Inicializacion de Listas basicas */

struct ListaBasica* ListaConstantesDecimales = NULL;
struct ListaBasica* ListaConstantesCaracter = NULL;
struct ListaBasica* ListaLiteralesCadena = NULL;

/* Fin Inicializacion*/

/**
* Crea la estructura en la que se almacenan las constantes decimales
* que fueron reconocidas por el analizador lexico
* @param {char*} yytext - Valor de la literal cadena reconocida
*/
void crearEstructuraConstanteDecimales(char* yytext){
    struct ListaBasica* nuevaConstanteDecimal = (struct ListaBasica*)malloc(sizeof(struct ListaBasica));
    nuevaConstanteDecimal->constante = strdup(yytext);
    nuevaConstanteDecimal->siguiente = ListaConstantesDecimales;

    ListaConstantesDecimales = nuevaConstanteDecimal;
}

/**
* Imprime en un fichero de salida el reporte de las constantes decimales encontrados
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirConstantesDecimales(FILE* salida){
    fprintf(salida, "%sListado de Constantes Decimales\n\n", generarEspacios(8));
    struct ListaBasica* actual = ListaConstantesDecimales;
    int valorTotal= 0;
    while (actual != NULL) {
        int valor = baseADecimal(actual->constante, 10);
        fprintf(salida, "Constante Decimal: %s\n", actual->constante);
        valorTotal += valor;
        struct ListaBasica* aux = actual;
        actual = actual->siguiente;
        free(aux->constante);
        free(aux);
    }

    fprintf(salida,"Total acumulado de las variables decimales: %i\n", valorTotal);
}

/**
* Dado un node la lista de literales cadena, devuelve el largo de su valor
* @param {struct ListaBasica*} literalCadena
* @returns {int} 
*/
int largoLiteralCadena(struct ListaBasica* literalCadena) {
    return strlen(literalCadena->constante) - 2; //Las comillas
}

/**
* Crea la estructura en la que se almacenan los literales cadena
* que fueron reconocidas por el analizador lexico
* @param {char*} yytext - Valor de la literal cadena reconocida
*/
void crearEstructuraLiteralesCadena(char* yytext) {
    struct ListaBasica* nuevoLiteralCadena = (struct ListaBasica*)malloc(sizeof(struct ListaBasica));

    nuevoLiteralCadena->constante = strdup(yytext);

    nuevoLiteralCadena->siguiente = NULL;
    
    if (ListaLiteralesCadena == NULL || largoLiteralCadena(nuevoLiteralCadena) < largoLiteralCadena(ListaLiteralesCadena)) {
        nuevoLiteralCadena->siguiente = ListaLiteralesCadena;
        ListaLiteralesCadena = nuevoLiteralCadena;
    } else {
        struct ListaBasica* actual = ListaLiteralesCadena;
        while(actual->siguiente != NULL && largoLiteralCadena(nuevoLiteralCadena) > largoLiteralCadena(actual->siguiente)) {
            actual = actual->siguiente;
        }
        nuevoLiteralCadena->siguiente = actual->siguiente;
        actual->siguiente = nuevoLiteralCadena;
    }

}

/**
* Imprime en un fichero de salida el reporte de Literales Cadena encontrados
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirListaLiteralesCadena(FILE* salida) {
    fprintf(salida, "%sListado de Literales Cadena\n\n", generarEspacios(8));

    struct ListaBasica* actual = ListaLiteralesCadena;
    while (actual != NULL) {
        int largo = strlen(actual->constante) - 2;
        fprintf(salida, "Literal Cadena: %s - Longitud: %d\n", actual->constante, largo);
        struct ListaBasica* aux = actual;
        actual = actual->siguiente;
        free(aux->constante);
        free(aux);
    }
}

/**
* Crea la estructura en la que se almacenan las constantes de tipo caracter
* que fueron reconocidas por el analizador lexico
* @param {char*} yytext - Valor de la constante caracter reconocida
*/
void crearEstructuraConstanteCaracter(char* yytext) {

    struct ListaBasica* nuevoConstanteCaracter = (struct ListaBasica*)malloc(sizeof(struct ListaBasica));
    nuevoConstanteCaracter->constante = strdup(yytext);
    nuevoConstanteCaracter->siguiente = ListaConstantesCaracter;
    ListaConstantesCaracter = nuevoConstanteCaracter;
}

/**
* Dada una lista del tipo basica, la da vuelta
* @param {struct ListaBasic**} Lista 
*/
void darVueltaLista(struct ListaBasica** Lista) {
    struct ListaBasica* actual = *Lista;
    struct ListaBasica *anterior = NULL, *siguiente = NULL;
 
    while (actual != NULL) {
        siguiente = actual->siguiente;
        actual->siguiente = anterior;
        anterior = actual;
        actual = siguiente;
    }
    *Lista = anterior;
}

/**
* Imprime en un fichero de salida el reporte de Contantes Caracteres encontrados
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirConstantesCaracter(FILE* salida) {
    darVueltaLista(&ListaConstantesCaracter);
    fprintf(salida, "%sListado de Constantes Caracter\n\n", generarEspacios(8));
    struct ListaBasica* actual = ListaConstantesCaracter;
    int aparicion = 1;
    while (actual != NULL) {
        fprintf(salida, "Orden de aparicion: %i - Constante caracter: %s\n", aparicion, actual->constante);
        struct ListaBasica* aux = actual;
        actual = actual->siguiente;
        free(aux->constante);
        free(aux);
        aparicion++;
    }
}
