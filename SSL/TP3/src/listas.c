#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "listas.h"




void agregarIdentificador(char identificador[100]) {
    ListaId* nuevoNodo = malloc(sizeof(ListaId));
    strcpy(nuevoNodo->identificador, identificador);
    nuevoNodo->siguiente = NULL;

    if (ListaIdentificadores == NULL) {
        ListaIdentificadores = nuevoNodo;
    } else {
        ListaId* ultimo = ListaIdentificadores;
        while (ultimo->siguiente != NULL) {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nuevoNodo;
    }
}


void agregarDeclaraciones(TipoVariable tipo) {
    ListaId* actual = ListaIdentificadores;

    while(actual) {
        ListaDeclaracion* nuevoNodo = malloc(sizeof(ListaDeclaracion));
        strcpy(nuevoNodo->identificador, actual->identificador);
        nuevoNodo->tipo = tipo;
        nuevoNodo->siguiente = NULL;

        if (ListaVariablesDeclaradas == NULL) {
            ListaVariablesDeclaradas = nuevoNodo;
        } else {
            ListaDeclaracion* ultimo = ListaVariablesDeclaradas;
            while (ultimo->siguiente != NULL) {
                ultimo = ultimo->siguiente;
            }
            ultimo->siguiente = nuevoNodo;
        }
        ListaId* aux = actual;
        actual = actual->siguiente;
        free(aux);
    }

    ListaIdentificadores = NULL;
    
}



char* obtenerTipoDeDato(TipoVariable tipo) {
    switch (tipo)
    {
        case TIPO_CHAR:
            return "char";
        case TIPO_DOUBLE:
            return "double";
        case TIPO_FLOAT:
            return "float";
        case TIPO_INT:
            return "int";
        case TIPO_LONG:
            return "long";
        case TIPO_SHORT:
            return "short";
        case TIPO_VOID:
            return "void";
    }
}

void reporteVariableDeclaradas() {
    printf("\n\n");
    printf("Listado de Variables declaradas:\n");
    
    ListaDeclaracion* actual = ListaVariablesDeclaradas;

    while (actual != NULL) {
        printf("Variable declarada con tipo \"%s\", identificador \"%s\"\n",obtenerTipoDeDato(actual->tipo), actual->identificador);
        ListaDeclaracion* aux = actual;
        actual = actual->siguiente;
        free(aux);
    };
    
}

void agregarSentencia(char* tipo, short linea) {
    ListaSent* nuevoNodo = malloc(sizeof(ListaSent));
    nuevoNodo->tipo = tipo;
    nuevoNodo->linea = linea;
    nuevoNodo->siguiente = NULL;

    if (ListaSentencias == NULL) {
        ListaSentencias = nuevoNodo;
    } else {
        ListaSent* ultimo = ListaSentencias;
        while (ultimo->siguiente != NULL) {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nuevoNodo;
    }
}
void reporteSentencias() {
    printf("\n\n");
    printf("Listado de Sentencias encontradas:\n");
    
    ListaSent* actual = ListaSentencias;

    while (actual != NULL) {
        printf("Sentencia de tipo \"%s\" encontrada en linea %i\n", actual->tipo, actual->linea);
        ListaSent* aux = actual;
        actual = actual->siguiente;
        free(aux);
    };
}


// Estructuras No Reconocidas
void agregarNoReconocido(short esLexico, short linea) {
    ListaNoRec* nuevoNodo = malloc(sizeof(ListaNoRec));
    nuevoNodo->esLexico = esLexico;
    nuevoNodo->linea = linea;
    nuevoNodo->siguiente = NULL;

    if (ListaNoReconocido == NULL) {
        ListaNoReconocido = nuevoNodo;
    } else {
        ListaNoRec* ultimo = ListaNoReconocido;
        while (ultimo->siguiente != NULL) {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nuevoNodo;
    }
}

void reporteNoReconocido() {
    printf("\n\n");
    printf("Listado de Estructuras o caracteres NO reconocidos:\n");
    
    ListaNoRec* actual = ListaNoReconocido;
    char* tipo;
    while (actual != NULL) {
        tipo = (actual->esLexico) ? "Lexico" : "Sintactico" ;
        printf("No reconocido en linea: %i, de tipo: %s\n", actual->linea, tipo);
        ListaNoRec* aux = actual;
        actual = actual->siguiente;
        free(aux);
    };
}


// Funciones reconocidas
void agregarFuncion(char identificador[100], char retorno[10], short cantArgm) {
    ListaFunc* nuevoNodo = malloc(sizeof(ListaFunc));
    strcpy(nuevoNodo->identificador, identificador);
    strcpy(nuevoNodo->retorno, retorno);
    nuevoNodo->cantArgumentos = cantArgm;

    if (ListaFunciones == NULL) {
        ListaFunciones = nuevoNodo;
    } else {
        ListaFunc* ultimo = ListaFunciones;
        while (ultimo->siguiente != NULL) {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nuevoNodo;
    }
}
void reporteFunciones() {
    printf("\n\n");
    printf("Listado de Funciones definidas:\n");
    
    ListaFunc* actual = ListaFunciones;
    while (actual != NULL) {
        
        printf("Funcion \"%s\" con tipo de retorno: %s, cantidad de argumentos %i\n", actual->identificador,  actual->retorno, actual->cantArgumentos);
        ListaFunc* aux = actual;
        actual = actual->siguiente;
        free(aux);
    };
}

