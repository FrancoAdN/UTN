struct PalabrasReservadas{
    char* palabraReservada;
    char* tipoPalabra;
    struct PalabrasReservadas* siguiente;
};

struct PalabrasReservadas* ListaPalabrasReservadas = NULL;

/**
* Crea la estructura en la que se almacenan las palabras reservadas
* que fueron reconocidas por el analizador lexico
* @param {char*} yytext - Valor de la literal cadena reconocida
* @param {char*} tipo   - Tipo de palabra reservada
*/
void crearEstructuraPalabrasReservadas(char* yytext, char* tipo){
    struct PalabrasReservadas* nuevaPalabraReservada = (struct PalabrasReservadas*)malloc(sizeof(struct PalabrasReservadas));
    nuevaPalabraReservada->palabraReservada = strdup(yytext);
    nuevaPalabraReservada->tipoPalabra = strdup(tipo);
    nuevaPalabraReservada->siguiente = ListaPalabrasReservadas;
    ListaPalabrasReservadas = nuevaPalabraReservada;
}

void darVueltaListaReservada(struct PalabrasReservadas** Lista) {
    struct PalabrasReservadas* actual = *Lista;
    struct PalabrasReservadas *anterior = NULL, *siguiente = NULL;
 
    while (actual != NULL) {
        siguiente = actual->siguiente;
        actual->siguiente = anterior;
        anterior = actual;
        actual = siguiente;
    }
    *Lista = anterior;
}
/**
* Imprime en un fichero de salida el reporte de las palabras reservadas encontradas
* @param {FILE*} salida - Fichero en el cual escribir
*/
void imprimirListaPalabrasReservadas(FILE* salida){
    darVueltaListaReservada(&ListaPalabrasReservadas);
    fprintf(salida, "%sListado de Palabras Reservadas\n\n", generarEspacios(8));
    struct PalabrasReservadas* actual = ListaPalabrasReservadas;
    while (actual != NULL) {
        fprintf(salida,"Palabra Reservada: %s - Tipo de Palabra Reservada: %s\n", actual->palabraReservada, actual->tipoPalabra);
        struct PalabrasReservadas* aux = actual;
        actual = actual->siguiente;
        free(aux->palabraReservada);
        free(aux);
    }
}