%{  

	#define YYDEBUG 1
	#include <stdio.h>
	#include <stdio.h>
	#include <ctype.h>
	#include <string.h>
    #include "listas.h"
    extern int yylineno;


	int yylex();
	int yywrap(){
		return(1);
	}


	short flag_error = 0;
    short cantArgumentos = 0;
	TipoVariable datoDeclarado;


%}

%union { 

    char valor[100];
}

%token <valor> LITERAL_CADENA CONST_CARACTER CONST_REAL CONST_HEX CONST_OCT CONST_DEC
%token CHAR DOUBLE FLOAT INT LONG SHORT VOID SIGNED UNSIGNED
%token IF ELSE WHILE DO SWITCH FOR CASE DEFAULT PALABRARESERVADA
%token BREAK RETURN GOTO CONTINUE
%token COMENTARIO
%token <valor> IDENTIFICADOR

%token SIZEOF
%token MENOS_MENOS MAS_MAS



%left '=' MAS_IGUAL MENOS_IGUAL POR_IGUAL
%right OR
%right AND
%right '<' '>' MAYORIGUAL MENORIGUAL
%right DESIGUALDAD IGUALDAD
%right '+' '-'
%right '*' '/' '%'
%right '^'
 
%start input

%%

input: /* vacio */
    | input line
;

line: '\n'
    | sentencia '\n'
    | sentencia 
;




//  SENTENCIAS

sentencia: sentCompuesta 
        | sentExpresion 
        | sentSeleccion  
        | sentIteracion 
        | sentSalto  
        | declaracion
        | '\n'
        | definicionFuncion
        | error 
        ;

sentCompuesta: '{' listaDeclaraciones listaSentencias '}' { agregarSentencia("Compuesta", yylineno);}
            ;


listaDeclaraciones: declaracion
                | listaDeclaraciones declaracion
				| /* vacio */
                ;

listaSentencias: sentencia
              | listaSentencias sentencia
			  | /* vacio */
              ;

sentExpresion: expresion ';' { agregarSentencia("Expresion", yylineno);}
            ;

sentSeleccion: IF '(' expresion ')' sentencia { agregarSentencia("Seleccion", yylineno);}
             | IF '(' expresion ')' sentencia ELSE sentencia { agregarSentencia("Seleccion", yylineno);}
            //  | SWITCH '(' expresion ')' sentencia
             ;

sentIteracion: WHILE '(' expresion ')' sentencia { agregarSentencia("Iteracion (while)", yylineno);}
             | DO sentencia WHILE '(' expresion ')' ';' { agregarSentencia("Iteracion (do-while)", yylineno);}
             | FOR '(' expresion ';' expresion ';' expresion')' { agregarSentencia("Iteracion (for)", yylineno);}
;

// sentEtiquetada: CASE expresion (deberia ser constante) ':' sentencia
//             | DEFAULT sentencia
//             | IDENTIFICADOR ':' sentencia

sentSalto: CONTINUE ';' { agregarSentencia("Salto (continue)", yylineno);}
        | BREAK ';' { agregarSentencia("Salto (break)", yylineno);}
        | RETURN expresion ';' { agregarSentencia("Salto (return)", yylineno);}
        | GOTO IDENTIFICADOR ';'{ agregarSentencia("Salto (goto)", yylineno);}
;





// DECLARACIONES

listaDeclaracion:  /* Vacio */
	| declaracion ';' listaDeclaracion

;

declaracion: declaracionVarSimples
;

declaracionVarSimples: tipoDeDato listaVarSimples ';' {agregarDeclaraciones(datoDeclarado); flag_error = 0;}
                     | tipoDeDato IDENTIFICADOR '(' lista_argumentos ')' sentCompuesta { agregarFuncion($<valor>2, $<valor>1, cantArgumentos);cantArgumentos = 0;}
					 | error caracterDeCorte { flag_error = 1;}
;

tipoDeDato: CHAR 	{ datoDeclarado = TIPO_CHAR; }
	  | DOUBLE	{ datoDeclarado = TIPO_DOUBLE; }
	  | FLOAT	{ datoDeclarado = TIPO_FLOAT; }
	  | INT		{ datoDeclarado = TIPO_INT; }
	  | LONG	{ datoDeclarado = TIPO_LONG; }
	  | SHORT	{ datoDeclarado = TIPO_SHORT; }
;


listaVarSimples: unaVarSimple
				| unaVarSimple ',' listaVarSimples
                | error { flag_error = 1; free(ListaIdentificadores); ListaIdentificadores = NULL;}
;
unaVarSimple: IDENTIFICADOR { if (!flag_error) agregarIdentificador($<valor>1); else flag_error = 0;} ',' unaVarSimple
			| IDENTIFICADOR { if (!flag_error) agregarIdentificador($<valor>1); else flag_error = 0;}
            | IDENTIFICADOR '=' expresion  { if (!flag_error) agregarIdentificador($<valor>1); else flag_error = 0;}
			| error {  flag_error = 1; free(ListaIdentificadores); ListaIdentificadores = NULL;}
;



definicionFuncion: tipoDeRetorno IDENTIFICADOR '(' lista_argumentos ')' sentCompuesta { agregarFuncion($<valor>2, $<valor>1, cantArgumentos);cantArgumentos = 0;}
                ;

lista_argumentos: argumento ',' lista_argumentos
                | argumento
                | /* vacio */
                ;
argumento: tipoDeDato IDENTIFICADOR {cantArgumentos++;} 
;


tipoDeRetorno: CHAR 	
            | DOUBLE	
            | FLOAT	
            | INT		
            | SHORT	
            | VOID 
            ;




// EXPRESIONES
expresion: expAsignacion
        | expresion ',' expAsignacion
;

expAsignacion: expCondicional
            | expUnaria operAsignacion expAsignacion 
            ;

operAsignacion: '=' 
              | MAS_IGUAL
			  | MENOS_IGUAL
			  | POR_IGUAL
              ;

expCondicional: expOr
             | expOr OR expCondicional
             | expOr '?' expresion ':' expCondicional  
             ;

expOr: expAnd
     | expOr OR expAnd
     ;

expAnd: expIgualdad
      | expAnd AND expIgualdad
      ;

expIgualdad: expRelacional
          | expIgualdad IGUALDAD expRelacional
          | expIgualdad DESIGUALDAD expRelacional
          ;

expRelacional: expAditiva
            | expRelacional '<' expAditiva
            | expRelacional MENORIGUAL expAditiva
            | expRelacional '>' expAditiva
            | expRelacional MAYORIGUAL expAditiva
            ;

expAditiva: expMultiplicativa
         | expAditiva '+' expMultiplicativa
         | expAditiva '-' expMultiplicativa
         ;

expMultiplicativa: expUnaria
                | expMultiplicativa '*' expUnaria
                | expMultiplicativa '/' expUnaria
                | expMultiplicativa '%' expUnaria
                ;

expUnaria: expPostfijo 
         | MAS_MAS expUnaria
         | MENOS_MENOS expUnaria
         | expUnaria MAS_MAS
         | expUnaria MENOS_MENOS
         | operUnario expUnaria
         | SIZEOF '(' tipoDeDato ')'
         ;

operUnario: '&'
          | '*'
          | '-'
          | '!'
          ;

expPostfijo: expPrimaria
          | expPostfijo '[' expresion ']' 
          | expPostfijo '(' listaArgumentos ')'
          ;

listaArgumentos: expAsignacion
              | listaArgumentos ',' expAsignacion
              | /* vacio */
              ;

expPrimaria: IDENTIFICADOR  
           | CONST_DEC 
           | CONST_OCT 
           | CONST_HEX 
           | CONST_REAL 
           | CONST_CARACTER 
           | LITERAL_CADENA 
           | '(' expresion ')'
           ;



caracterDeCorte:	';' | '\n'


%%

int yyerror(const char *msg) {
    agregarNoReconocido(0, yylineno);
    return 1;
}