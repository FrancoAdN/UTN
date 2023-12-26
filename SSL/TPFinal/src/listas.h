#include "semantico.h"


typedef struct ListaId {
    ID identificador;
    struct ListaId* siguiente;
} ListaId;

typedef struct ListaSent
{
    char* tipo;
    short linea;
    struct ListaSent* siguiente;
} ListaSent;

typedef struct ListaDeclaracion {
    ID identificador;
    TipoVariable tipo;
    struct ListaDeclaracion* siguiente;
} ListaDeclaracion;


typedef struct ListaNoRec {
    short linea;
    short esLexico;
    struct ListaNoRec* siguiente;
} ListaNoRec;

typedef struct doblesDeclaraciones{
    ID identifier;
    int linea;
    struct doblesDeclaraciones* siguiente;
}doblesDeclaraciones;

typedef struct ListaFunc
{
    ID identificador;
    char retorno[10];
    short cantArgumentos;
    struct ListaFunc* siguiente;
} ListaFunc;


doblesDeclaraciones* listaDobles;
ListaDeclaracion* ListaVariablesDeclaradas;
ListaSent* ListaSentencias;
ListaId* ListaIdentificadores;
ListaNoRec* ListaNoReconocido;
ListaFunc* ListaFunciones;


// Variables encontradas
void agregarDeclaraciones(TipoVariable);



void reporteVariableDeclaradas();


void agregarIdentificador(ID);


// Sentencias encontradas
void agregarSentencia(char*, short );
void reporteSentencias();


// Estructuras No Reconocidas
void agregarNoReconocido(short, short);
void reporteNoReconocido();

// Funciones encontradas
void agregarFuncion(ID, char[10], short);
void reporteFunciones();
