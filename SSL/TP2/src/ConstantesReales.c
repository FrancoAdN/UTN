struct ConstantesReales{
    char* constanteReal;
    float valorMantisa;
    int valorEntera;
    struct ConstantesReales* siguiente;
};
struct ConstantesReales* ListaConstantesReales = NULL;

/**
* Crea la estructura en la que se almacenan las constantes reales
* que fueron reconocidas por el analizador lexico
* @param {char*} yytext - Valor de la literal cadena reconocida
*/
void crearEstructuraConstanteReales(char* yytext){
    float valorReal = 0;
    int parteEntera;
    float parteDecimal;
    struct ConstantesReales* nuevaConstanteReal = (struct ConstantesReales*)malloc(sizeof(struct ConstantesReales));
    nuevaConstanteReal->constanteReal = strdup(yytext);
    valorReal = atof(yytext);
    parteEntera = (int) valorReal;
    parteDecimal = valorReal - parteEntera;
    nuevaConstanteReal->valorMantisa=parteDecimal;
    nuevaConstanteReal->valorEntera=parteEntera;
    nuevaConstanteReal->siguiente = ListaConstantesReales;
    ListaConstantesReales = nuevaConstanteReal;

}

/**
* Imprime en un fichero de salida el reporte de las Constantes reales encontradas
* @param {FILE*}                salida - Fichero en el cual escribir
*/
void imprimirConstantesReales(FILE* salida){
    fprintf(salida, "%sListado de Constantes Reales\n\n", generarEspacios(8));
    struct ConstantesReales* actual = ListaConstantesReales;
    while (actual != NULL) {
        fprintf(salida,"Constante Real: %s  Valor Entero: %d  Valor Mantisa: %f\n", actual->constanteReal,actual->valorEntera,actual->valorMantisa);
        struct ConstantesReales* aux = actual;
        actual = actual->siguiente;
        free(aux->constanteReal);
        free(aux);
    }

}