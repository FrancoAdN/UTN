#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>

static const char INPUT_FILE[] = "entrada.txt";
static const char OUTPUT_FILE[] = "salida.txt";

typedef struct Nodo Nodo;

enum Estados {
  Hexa = 1,
  Oct = 2,
  Dec = 3,
  Error = 4
};

struct Nodo {
  char intervalo[2];
  int id;
  Nodo *izquierda;
  Nodo *derecha;
};

/*
 * Crea un nuevo nodo
 * @params {char[2]} - intervalos de caracteres que acepta el estado a crear
 * @params {int}     - estado numerico
 * @returns {Nodo}   - nodo creado
*/
Nodo* nuevoNodo(char intervalo[2], int id) {
  Nodo* nodo = (Nodo*) malloc(sizeof(Nodo));
  nodo->id = id;

  nodo->intervalo[0] = intervalo[0];
  nodo->intervalo[1] = intervalo[1];
  nodo->izquierda = NULL;
  nodo->derecha = NULL;

  return nodo;
}

/*
 * Dado un valor de estado, retorna su palabra equivalente
 * @param {int}     - estado numerico
 * @returns {char*} - palabra equivalente
*/
char* tipoDeConstante(int id) {
  switch (id)
  {
    case Hexa:
      return "HEXADECIMAL";
    case Oct:
      return "OCTAL";
    case Dec:
      return "DECIMAL";
    case Error:
      return "NO RECONOCIDA";
  }
}

/*
 * Imprime un estado final al archivo de salida
 * @params {FILE} - archivo de salida
 * @params {int}  - estado numerico a imprimir
 * @params {int}  - cantidad de espacios usados
*/
void imprimirEstadoFinal(FILE *file, int id, int cont) {
  for (int i = cont; i < 20; i++) {
    fputc(' ', file);
  }
  fputs(tipoDeConstante(id), file);
  fputc('\n', file);
}

/*
 * Inicializa el arbol de transiciones
 * @returns {Nodo} - raiz del arbol inicializado
*/
Nodo* inicializarArbol() {

  // Nodos para HEXADECIMAL
  Nodo* q7 = nuevoNodo("af", Hexa);
  q7->izquierda = q7;
  
  Nodo* q6 = nuevoNodo("09", Hexa);
  q6->izquierda = q7;
  q6->derecha = q6;
  q7->derecha = q6;

  Nodo* q5 = nuevoNodo("x", Hexa);
  q5->izquierda = q6;
  q5->derecha = q7;

  // Nodos para OCTAL
  Nodo* q4 = nuevoNodo("07", Oct);
  q4->izquierda = q4;
  Nodo* q3 = nuevoNodo("0", Oct);
  q3->izquierda = q4;
  q3->derecha = q5;

  // Nodos para DECIMAL
  Nodo* q2 = nuevoNodo("09", Dec);
  q2->izquierda = q2;
  Nodo* q1 = nuevoNodo("19", Dec);
  q1->izquierda = q2;

  Nodo* inicial = (Nodo*) malloc(sizeof(Nodo));
  inicial->izquierda = q1;
  inicial->derecha = q3;

  return inicial;
}

/*
 * Verifica y obtiene el siguiente estado al cual puede transicionar
 * @param {Nodo}   - nodo con estado actual de la constante en procesamiento
 * @param {char}   - caracter actual a verificar
 * @param {Nodo}   - nodo error
 * @returns {Nodo} - nodo a transicionar
*/
Nodo* obtenerSiguiente(Nodo* actual, char caracter, Nodo* error) {

  if(actual->id == Error) {
    return actual;
  }


  if(actual->izquierda) {
    if(actual->izquierda->intervalo[1]) {
      if(caracter >= actual->izquierda->intervalo[0] && caracter <= actual->izquierda->intervalo[1]) {
        return actual->izquierda;
      }
    } else if (caracter == actual->izquierda->intervalo[0]) {
      return actual->izquierda;
    }
  }

  if(actual->derecha) {
    if(actual->derecha->intervalo[1]) {
      if(caracter >= actual->derecha->intervalo[0] && caracter <= actual->derecha->intervalo[1]) {
        return actual->derecha;
      }
  } else if (caracter == actual->derecha->intervalo[0]){
      return actual->derecha;
    }
  }

  return error;
}

int main() {


  Nodo* inicial = inicializarArbol();
  Nodo* actual = inicial;
 
  Nodo* error = (Nodo*) malloc(sizeof(Nodo));
  error->id = Error;


  FILE *entrada;
  FILE *salida;
  entrada = fopen(INPUT_FILE, "r");
  salida = fopen(OUTPUT_FILE, "w+");

  char centinela = ',';
  char caracter = fgetc(entrada);
  int contador = 0;

  while (caracter != EOF)
  { 

    caracter = tolower(caracter);
    if (caracter == centinela) {

      imprimirEstadoFinal(salida, actual->id, contador);
      actual = inicial;
      caracter = fgetc(entrada);
      contador = 0;
      continue;
    }

    contador++;
    fputc(caracter, salida);

    actual = obtenerSiguiente(actual, caracter, error);
    caracter = fgetc(entrada);
  }
  
  imprimirEstadoFinal(salida, actual->id, contador);

  fclose(entrada);
  fclose(salida);

  return 0;
}