
%{

#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
// #include "ConstantesHexadecimales.c"
#include "ConstantesNoDecimales.c"
#include "ListaBasica.c"
#include "ListaContador.c"
#include "Comentarios.c"
#include "PalabrasReservadas.c"
#include "ConstantesReales.c"
#include "NoReconocidos.c"

%}



%option noyywrap
%option yylineno



/*Seccion Tokens*/
DIGITODECIMAL               [1-9]
DIGITOOCTAL                 [0-7]
DIGITOHEXADECIMAL           [0-9a-fA-F]
DIGITO                      [0-9]
INICIO_COMENTARIO_SIMPLE    "//"

LETRAS                      [a-zA-Z]
IDENTIFICADOR               [a-zA-Z_]+[a-zA-Z0-9_]*
COMILLAS_SIMPLE             "\'"
COMILLAS_DOBLES             "\""

/*Operadores Logicos y aritmeticos basicos*/
OP_SUMA                     "+"
OP_RESTA                    "-"
OP_MULTIPLICACION           "*"
OP_DIVISION                 "/"
OP_IGUAL                    "="
OP_MAYOR                    ">"
OP_MENOR                    "<"
OP_NEGACION                 "!"
OP_COMA                     ","
DOS_PUNTOS                  ":"
PUNTO_Y_COMA                ";"
OP_CONDICIONAL              "?"
OP_AND                      "&&"    
OP_OR                       "||"
OP_REFERENCIA               "&"
OP_ELIPSIS                  "..."
INICIO_PAR                  "("
FIN_PAR                     ")"
INICIO_CORC                 "["
FIN_CORC                    "]"
INICIO_LLAV                 "{"
FIN_LLAV                    "}"


/*Palabras Reservadas*/

/*Palabra Reservada - Tipo de Dato*/
PR_CHAR                 "char"
PR_INT                  "int"
PR_SHORT                "short"
PR_FLOAT                "float"
PR_UNSIGNED             "unsigned"
PR_CONST                "const"
PR_SIGNED               "signed"
PR_LONG                 "long"
PR_DOUBLE               "double"
PR_ENUM                 "enum"
PR_TYPEDEF              "typedef"
PR_VOLATILE             "volatile"
PR_IMAGINARY            "_Imaginary"
PR_COMPLEX              "_Complex"
PR_BOOL                 "_Bool"
PR_TIPO_DE_DATO         {PR_CHAR}|{PR_INT}|{PR_SHORT}|{PR_FLOAT}|{PR_UNSIGNED}|{PR_CONST}|{PR_SIGNED}|{PR_LONG}|{PR_DOUBLE}|{PR_ENUM}|{PR_TYPEDEF}|{PR_VOLATILE}|{PR_IMAGINARY}|{PR_COMPLEX}|{PR_BOOL}

/*Palabra Reservada - Estructuras de control*/
PR_IF                   "if"
PR_ELSE                 "else"
PR_FOR                  "for"
PR_WHILE                "while"
PR_DO                   "do"
PR_SWITCH               "switch"
PR_CASE                 "case"
PR_BREAK                "break"
PR_CONTINUE             "continue"
PR_GOTO                 "goto"
PR_DEFAULT              "default"
PR_ESTRUCTURA_CONTROL   {PR_IF}|{PR_ELSE}|{PR_FOR}|{PR_WHILE}|{PR_DO}|{PR_SWITCH}|{PR_CASE}|{PR_BREAK}|{PR_CONTINUE}|{PR_GOTO}|{PR_DEFAULT}

/*Palabra Reservada - Otros*/
PR_RETURN               "return"
PR_AUTO                 "auto"
PR_REGISTER             "register"
PR_STATIC               "static"
PR_EXTERN               "extern"
PR_SIZEOF               "sizeof"
PR_UNION                "union"
PR_STRUCT               "struct"
PR_VOID                 "void"
PR_OTROS                {PR_RETURN}|{PR_AUTO}|{PR_REGISTER}|{PR_STATIC}|{PR_EXTERN}|{PR_SIZEOF}|{PR_UNION}|{PR_STRUCT}|{PR_VOID}



%%

{DIGITODECIMAL}{DIGITO}*                            { crearEstructuraConstanteDecimales(yytext); }
0{DIGITOOCTAL}*                                     { crearEstructuraConstante(yytext, &ListaConstantesOctales, 8); }
0[xX]{DIGITOHEXADECIMAL}+                           { crearEstructuraConstante(yytext, &ListaConstantesHexadecimales, 16); }
(-?){DIGITO}*(\.){DIGITO}+                          { crearEstructuraConstanteReales(yytext); }
{COMILLAS_SIMPLE}{LETRAS}{COMILLAS_SIMPLE}          { crearEstructuraConstanteCaracter(yytext); }
{COMILLAS_DOBLES}(.*){COMILLAS_DOBLES}              { crearEstructuraLiteralesCadena(yytext);}


{PR_TIPO_DE_DATO}                                   { crearEstructuraPalabrasReservadas(yytext, "Tipo de dato"); }
{PR_ESTRUCTURA_CONTROL}                             { crearEstructuraPalabrasReservadas(yytext, "Estructura de control"); }
{PR_OTROS}                                          { crearEstructuraPalabrasReservadas(yytext, "Otra"); }


{IDENTIFICADOR}                                     {crearEstructuraListaContador(yytext, &ListaIdentificadores);}


{INICIO_COMENTARIO_SIMPLE}{1,10}(.*)                                                                        { crearEstructuraComentarios(yytext, &ListaComentariosSimple);}
{OP_DIVISION}{OP_MULTIPLICACION}+(.*)[ \r\t\n]*(.*)[ \r\t\n]*(.*){OP_MULTIPLICACION}+{OP_DIVISION}          { crearEstructuraComentarios(yytext, &ListaComentariosMultiples);}
     
{OP_COMA}                                           {crearEstructuraListaContador(yytext, &ListaOperadores);}
{DOS_PUNTOS}                                        {crearEstructuraListaContador(yytext, &ListaOperadores);}
{PUNTO_Y_COMA}                                      {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_SUMA}                                           {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_RESTA}                                          {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_MULTIPLICACION}                                 {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_DIVISION}                                       {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_IGUAL}                                          {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_MAYOR}                                          {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_MENOR}                                          {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_MAYOR}{OP_IGUAL}                                {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_MENOR}{OP_IGUAL}                                {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_IGUAL}{OP_IGUAL}                                {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_NEGACION}                                       {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_NEGACION}{OP_IGUAL}                             {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_SUMA}{2}                                        {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_RESTA}{2}                                       {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_CONDICIONAL}                                    {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_AND}                                            {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_OR}                                             {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_REFERENCIA}                                     {crearEstructuraListaContador(yytext, &ListaOperadores);}
{OP_ELIPSIS}                                        {crearEstructuraListaContador(yytext, &ListaOperadores);}
{INICIO_PAR}                                        {crearEstructuraListaContador(yytext, &ListaOperadores);}
{FIN_PAR}                                           {crearEstructuraListaContador(yytext, &ListaOperadores);}
{INICIO_CORC}                                       {crearEstructuraListaContador(yytext, &ListaOperadores);}
{FIN_CORC}                                          {crearEstructuraListaContador(yytext, &ListaOperadores);}
{INICIO_LLAV}                                       {crearEstructuraListaContador(yytext, &ListaOperadores);}
{FIN_LLAV}                                          {crearEstructuraListaContador(yytext, &ListaOperadores);}

[ \r\t\n]+                                            /*No deberia hacer nada*/
.                                                   { crearEstructuraNoReconocido(yytext, yylineno); }
%%

int main(int argc,char* argv[])
{	
    FILE *salida;
    salida = fopen("salida.txt", "w"); 

    yyin = fopen(argv[1],"r");

    yylex();
    
    printf("\n\n");

    imprimirListaIdentificadores(salida);
    fprintf(salida, "\n\n");

    imprimirListaLiteralesCadena(salida);
    fprintf(salida, "\n\n");

    imprimirListaPalabrasReservadas(salida);
    fprintf(salida, "\n\n");

    imprimirConstantes(&ListaConstantesOctales, 8, salida);
    fprintf(salida, "\n\n");
    imprimirConstantes(&ListaConstantesHexadecimales, 16, salida);
    fprintf(salida, "\n\n");

    imprimirConstantesDecimales(salida);
    fprintf(salida, "\n\n");

    imprimirConstantesReales(salida);
    fprintf(salida, "\n\n");

    imprimirConstantesCaracter(salida);
    fprintf(salida, "\n\n");

    imprimirListaOperadores(salida);
    fprintf(salida, "\n\n");
    
    imprimirComentarios(salida, &ListaComentariosSimple, 1);
    fprintf(salida, "\n\n");

    imprimirComentarios(salida, &ListaComentariosMultiples, 0);
    fprintf(salida, "\n\n");

    imprimirCaracteresNoReconocidos(salida);
    fprintf(salida, "\n\n");

    fclose(salida);

    return 0;
}
