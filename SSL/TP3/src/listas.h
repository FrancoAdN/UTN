
typedef enum {
	TIPO_CHAR,
	TIPO_DOUBLE,
	TIPO_FLOAT,
	TIPO_INT,
	TIPO_LONG,
	TIPO_SHORT,
    TIPO_VOID
} TipoVariable;

typedef struct ListaId {
    char identificador[100];
    struct ListaId* siguiente;
} ListaId;

typedef struct ListaSent
{
    char* tipo;
    short linea;
    struct ListaSent* siguiente;
} ListaSent;

typedef struct ListaDeclaracion {
    char identificador[100];
    TipoVariable tipo;
    struct ListaDeclaracion* siguiente;
} ListaDeclaracion;


typedef struct ListaNoRec {
    short linea;
    short esLexico;
    struct ListaNoRec* siguiente;
} ListaNoRec;

typedef struct ListaFunc
{
    char identificador[100];
    char retorno[10];
    short cantArgumentos;
    struct ListaFunc* siguiente;
} ListaFunc;



ListaDeclaracion* ListaVariablesDeclaradas;
ListaSent* ListaSentencias;
ListaId* ListaIdentificadores;
ListaNoRec* ListaNoReconocido;
ListaFunc* ListaFunciones;


// Variables encontradas
void agregarDeclaraciones(TipoVariable);

char* obtenerTipoDeDato(TipoVariable );

void reporteVariableDeclaradas();


void agregarIdentificador(char[100]);


// Sentencias encontradas
void agregarSentencia(char*, short );
void reporteSentencias();


// Estructuras No Reconocidas
void agregarNoReconocido(short, short);
void reporteNoReconocido();

// Funciones encontradas
void agregarFuncion(char[100], char[10], short);
void reporteFunciones();