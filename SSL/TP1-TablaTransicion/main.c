#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

static const char INPUT_FILE[] = "entrada.txt";
static const char OUTPUT_FILE[] = "salida.txt";

char* tipoDeConstante(int final) {
  switch (final)
  {
    case 1:
      return "OCTAL";
    case 2:
      return "DECIMAL";
    case 4:
      return "HEXADECIMAL";
    case 5:
      return "OCTAL";
    
    default:
      return "NO RECONOCIDA";
  }
    
}

int ObtenerSiguienteColumna(char intervalos[5][2], char caracter) {

  for (int i = 0; i < 5; i++) {
    if (caracter >= intervalos[i][0] && caracter <= intervalos[i][1]) {
      return i;
    }
  }
  return 5;

};


void imprimirEstadoFinal(FILE *file, int estadoFinal, int cont) {
  for (int i = cont; i < 20; i++) {
    fputc(' ', file);
  }
  fputs(tipoDeConstante(estadoFinal), file);
  fputc('\n', file);
}

int main() {

  int transiciones[7][6] = {
  /*          0  [1-7] [8-9] [a-f]  [x] [*]  */
  /* q0- */ { 1,  2,    2,    6,     6,  6},
  /* q1+ */ { 5,  5,    6,    6,     3,  6},
  /* q2+ */ { 2,  2,    2,    6,     6,  6},
  /* q3  */ { 4,  4,    4,    4,     6,  6},
  /* q4+ */ { 4,  4,    4,    4,     6,  6},
  /* q5+ */ { 5,  5,    6,    6,     6,  6},
  /* q6  */ { 6,  6,    6,    6,     6,  6}
  };

  char intervalos[5][2] = {
    {'0', '0'},
    {'1', '7'},
    {'8', '9'},
    {'a', 'f'},
    {'x', 'x'}
  };

  FILE *entrada;
  FILE *salida;

  entrada = fopen(INPUT_FILE, "r");
  salida = fopen(OUTPUT_FILE, "w");

  char centinela = ',';
  char caracter = fgetc(entrada);
  int contador = 0;
  int estadoActual = 0;
  char minuscula;

  while (caracter != EOF) {
    minuscula = tolower(caracter);

    if (caracter == centinela) {
      imprimirEstadoFinal(salida, estadoActual, contador);
      estadoActual = 0;
      caracter = fgetc(entrada);
      contador = 0;
      continue;
    }
    
    if (estadoActual != 6) {
      int columna = ObtenerSiguienteColumna(intervalos, minuscula);
      estadoActual = transiciones[estadoActual][columna];
    }

    contador++;
    fputc(caracter, salida);
    caracter = fgetc(entrada);
   
  }

  imprimirEstadoFinal(salida, estadoActual, contador);
  
  fclose(entrada);
  fclose(salida);
  
  return 0;
}