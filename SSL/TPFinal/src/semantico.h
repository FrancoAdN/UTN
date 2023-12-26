typedef char ID[100];

typedef enum {
	TIPO_CHAR,
	TIPO_DOUBLE,
	TIPO_FLOAT,
	TIPO_INT,
	TIPO_LONG,
	TIPO_SHORT,
    TIPO_VOID
} TipoVariable;

typedef struct ListaParam
{
    ID param;
    TipoVariable tipo;
    struct ListaParm* siguiente;
} ListaParam;

typedef struct TS
{
    ID identificador;
    TipoVariable tipo;
    short es_variable;
    short linea;
    ListaParam* params;
    struct TS* siguiente;
} TS;

typedef struct ListaArgs
{
    ID valor;
    short es_id;
    struct ListaArgs* siguiente;
} ListaArgs;


typedef struct ListaErrores {
    short linea;
    char* msg;
    char* tipo;
    struct ListaErrores* siguiente;
} ListaErrores;




TS* ts;
ListaParam* LParams;
ListaErrores* LErrores;
ListaArgs* LArgs;

void agregarDeclaracion(ID, TipoVariable, short, short, ListaParam*);
TS* buscarPorId(ID);
void eliminarPorLinea(short);
void imprimirTS();
void agregarParametro(TipoVariable, ID);
TipoVariable aEnum(char*);
void agregarError(char*, char*, short);
void validarAsignacion(ID, short, short);
void imprimirListadoErrores();

void agregarArgumento(char*, short);
void validarArgumentos(ID, short);
char* obtenerTipoDeDato(TipoVariable );