#include "semantico.h"
#include <string.h>

void agregarDeclaracion(ID id, TipoVariable tipo, short es_var, short linea, ListaParam* params) {
    TS* nodo = malloc(sizeof(TS));
    // printf("Agregar declaracion id %s, tipo %i\n", id, tipo);
    strcpy(nodo->identificador, id);
    nodo->tipo = tipo;
    nodo->es_variable = es_var;
    nodo->linea = linea;
    nodo->params = params;
    nodo->siguiente = NULL;

    LParams = NULL;

    if (ts == NULL) {
        ts = nodo;
    } else {
        TS* ultimo = ts;
        while (ultimo->siguiente)
        {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nodo;
    }

}

TS* buscarPorId(ID id) {
    TS* act = ts;
    while (act) {

        if (strcoll(act->identificador, id) == 0) {
            // printf("Existe identificador \"%s\" linea %i \n", id, act->linea);
            return act;
        }
        act = act->siguiente;
    }
    // printf("No existe identificador \"%s\"\n", id);
    return NULL;
}
char* obtenerTipoDeDato(TipoVariable tipo) {
    // printf("Tipo: %i\n", tipo);
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

void imprimirTS() {
    printf("\n\n");
    printf("Listado de Declaraciones:\n");
    TS* act = ts;
    while (act) {

        if (act->es_variable) {
            printf("Declaracion de variable: ");
            printf("identificador \"%s\", ", act->identificador);
            printf("tipo: \"%s\" ", obtenerTipoDeDato(act->tipo));
            printf("en linea: %i\n", act->linea);
        } else {
            printf("Declaracion de funcion: ");
            printf("identificador \"%s\", ", act->identificador);
            printf("tipo: \"%s\" ", obtenerTipoDeDato(act->tipo));
            printf("en linea: %i\n", act->linea);
            ListaParam* act_param = act->params;
            while (act_param) {
                printf("\t Parametro: tipo %s identificador \"%s\"\n", obtenerTipoDeDato(act_param->tipo), act_param->param);
                act_param = act_param->siguiente;
            }
        }
        
        TS* aux = act;
        act = act->siguiente;
        free(aux);
    }

}

void eliminarPorLinea(short linea) {

    TS* act = ts;
    while (act != NULL) {
        while (act->siguiente != NULL && act->siguiente->linea == linea) {
            TS *temp = act->siguiente;
            act->siguiente = temp->siguiente;
            free(temp);
        }
        act = act->siguiente;
    }
}


void agregarParametro(TipoVariable tipo, ID id) {
    ListaParam* nodo = malloc(sizeof(ListaParam));
    nodo->tipo = tipo;
    strcpy(nodo->param, id);
    nodo->siguiente = NULL;

    if (!LParams) {
        LParams = nodo;
    } else {
        ListaParam* ultimo = LParams;
        while (ultimo->siguiente)
        {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nodo; 
    }
}

TipoVariable aEnum(char* tipo) {
    // printf("A ENUM: %s %i\n", tipo, strlen(tipo));

    if (strcmp(tipo, "char") == 0) return TIPO_CHAR;
    if (strcmp(tipo, "int") == 0) return TIPO_INT;
    if (strcmp(tipo, "float") == 0) return TIPO_FLOAT;
    if (strcmp(tipo, "double")== 0) return TIPO_DOUBLE;
    if (strcmp(tipo, "void") == 0) return TIPO_VOID;
    if (strcmp(tipo, "short") == 0) return TIPO_SHORT;
    if (strcmp(tipo, "long") == 0) return TIPO_LONG;
}

void agregarError(char* msg, char* tipo, short linea) {
    ListaErrores* nodo = malloc(sizeof(ListaErrores));
    nodo->linea = linea;
    nodo->tipo = tipo;
    nodo->msg = msg;
    nodo->siguiente = NULL;

    if (!LErrores) {
        LErrores = nodo;
    } else {
        ListaErrores* ultimo = LErrores;
        while (ultimo->siguiente)
        {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nodo; 
    }
}

void validarAsignacion(ID id, short es_num, short linea) {
    TS* identificador = buscarPorId(id);
    if (!identificador) {
        agregarError("Identificador no declarado", "Semantico", linea);
        return 0;
    }


    if (!es_num) {
        agregarError("Expresion invalida", "Semantico", linea);
        return 0;
    }

    return 1;

}


void imprimirListadoErrores() {
    printf("\n\n");
    printf("Listado Errores:\n");
    ListaErrores* act = LErrores;
    while (act) {
        printf("Error %s: ", act->tipo);
        printf("en la linea %i, ", act->linea);
        printf("con mensaje: \"%s\"\n", act->msg);
        ListaErrores* aux = act;
        act = act->siguiente;
        free(aux);
    }
}

void agregarArgumento(char* arg, short es_id) {
    ListaArgs* nodo = malloc(sizeof(ListaArgs));
    strcpy(nodo->valor, arg);
    nodo->es_id = es_id;
    nodo->siguiente = NULL;

    if (!LArgs) {
        LArgs = nodo;
    } else {
        ListaArgs* ultimo = LArgs;
        while (ultimo->siguiente)
        {
            ultimo = ultimo->siguiente;
        }
        ultimo->siguiente = nodo; 
    }
}

void validarArgumentos(ID id, short linea) {
    TS* funcion = buscarPorId(id);

    if (!funcion) {
        agregarError("Identificador no reconocido", "Semantico", linea);
        return;
    }

    if (funcion->es_variable) {
        agregarError("Identificador de tipo variable", "Semantico", linea);
        return;
    }

    ListaParam* param = funcion->params;
    ListaArgs* arg = LArgs;

    while(param || arg) {

        if (param && !arg) {
            agregarError("Pocos argumentos", "Semantico", linea);
            return;
        }

        if (arg && !param) {
            agregarError("Demasiados argumentos", "Semantico", linea);
            return;
        }

        if (arg->es_id) {
            TS* id = buscarPorId(arg->valor);
            if (id->tipo != param->tipo) {
                agregarError("Argumento de tipo erroneo", "Semantico", linea);
                return;
            }
        }


        arg = arg->siguiente;
        param = param->siguiente;

    }
}

