/* Inicializacion de Listas comentarios*/

struct ListaBasica* ListaComentariosSimple = NULL;
struct ListaBasica* ListaComentariosMultiples = NULL;

/* Fin inicializacion Listas comentraios*/

/**
* Crea la estructura en la que se almacenan los comentraios
* que fueron reconocidas por el analizador lexico
* @param {char*}                yytext - Valor de la literal cadena reconocida
* @param {struct ListaBasica**} Lista  - Lista en la cual agregar el nuevo valor
*/
void crearEstructuraComentarios(char* yytext, struct ListaBasica** Lista) {
    struct ListaBasica* comentario = 
        (struct ListaBasica*)malloc(sizeof(struct ListaBasica));
    comentario->constante = strdup(yytext);
    comentario->siguiente = *Lista;
    *Lista = comentario;
}

/**
* Imprime en un fichero de salida el reporte de los comentarios encontrados
* @param {FILE*}                salida - Fichero en el cual escribir
* @param {struct ListaBasica**} Lista  - Lista a imprimir
* @param {int}                  simple - 1 si es comentario de simple linea. 0 si es comentario de multiple linea
*/
void imprimirComentarios(FILE* salida, struct ListaBasica** Lista, int simple) {
    if (simple) {
        fprintf(salida, "%sListado de Comentarios de una linea\n\n", generarEspacios(8));
    } else {
        fprintf(salida, "%sListado de Comentarios de multiples lineas\n\n", generarEspacios(8));
    }
    
    struct ListaBasica* actual = *Lista;
    while (actual != NULL) {
        fprintf(salida,"Comentario: %s\n", actual->constante);
        struct ListaBasica* aux = actual;
        actual = actual->siguiente;
        free(aux->constante);
        free(aux);
    }
}