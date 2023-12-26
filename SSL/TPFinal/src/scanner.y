%{  

	#define YYDEBUG 1
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
    struct {
        char valor[100];
        short es_numero;
        short es_id;
    } tkn;

}

%token <tkn> LITERAL_CADENA CONST_CARACTER CONST_REAL CONST_HEX CONST_OCT CONST_DEC
%token CHAR DOUBLE FLOAT INT LONG SHORT VOID SIGNED UNSIGNED
%token IF ELSE WHILE DO SWITCH FOR CASE DEFAULT PALABRARESERVADA
%token BREAK RETURN GOTO CONTINUE
%token COMENTARIO
%token <tkn> IDENTIFICADOR

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

sentencia: /* vacio */
        | sentCompuesta 
        | sentExpresion 
        | sentSeleccion  
        | sentIteracion 
        | sentSalto  
        | declaracion
        | definicionFuncion
        | caracterDeCorte
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
            | expAsignacion  { agregarSentencia("Expresion", yylineno);}
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

declaracionVarSimples: tipoDeDato listaVarSimples ';' {flag_error = 0;}
                    | tipoDeDato IDENTIFICADOR '(' lista_parametros ')' sentCompuesta { if (!buscarPorId($<tkn.valor>2)) agregarDeclaracion($<tkn.valor>2, aEnum($<tkn.valor>1), 0,yylineno, LParams); else {flag_error = 0; agregarDoble($<tkn.valor>2,yylineno);} LParams = NULL;}
                    | error caracterDeCorte { flag_error = 1; eliminarPorLinea(yylineno);}
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
                | error { flag_error = 1; eliminarPorLinea(yylineno);}
;
unaVarSimple: IDENTIFICADOR { if (!buscarPorId($<tkn.valor>1)) agregarDeclaracion($<tkn.valor>1, datoDeclarado, 1, yylineno, NULL); else {flag_error = 0; agregarDoble($<tkn.valor>1, yylineno);}} ',' unaVarSimple
			| IDENTIFICADOR { if (!buscarPorId($<tkn.valor>1)) agregarDeclaracion($<tkn.valor>1, datoDeclarado, 1, yylineno, NULL); else {flag_error = 0; agregarDoble($<tkn.valor>1, yylineno);}}
            | IDENTIFICADOR '=' expresion  {if (!buscarPorId($<tkn.valor>1)) agregarDeclaracion($<tkn.valor>1, datoDeclarado, 1, yylineno, NULL); else {flag_error = 0; agregarDoble($<tkn.valor>1, yylineno);}}
			| error {  flag_error = 1;eliminarPorLinea(yylineno);}
;


definicionFuncion: tipoDeRetorno IDENTIFICADOR '(' lista_parametros ')' sentCompuesta { if (!buscarPorId($<tkn.valor>2)) agregarDeclaracion($<tkn.valor>2, aEnum($<tkn.valor>1), 0, yylineno, LParams); else {flag_error = 0; agregarDoble($<tkn.valor>2,yylineno);};LParams = NULL;}
                ;

lista_parametros: parametro ',' lista_parametros
                | parametro
                | /* vacio */
                ;
parametro: tipoDeDato IDENTIFICADOR {agregarParametro(datoDeclarado, $<tkn.valor>2);} 
;


tipoDeRetorno: CHAR 	{ datoDeclarado = TIPO_CHAR; }
            | DOUBLE	{ datoDeclarado = TIPO_DOUBLE; }
            | FLOAT	    { datoDeclarado = TIPO_FLOAT; }
            | INT		{ datoDeclarado = TIPO_INT; }
            | SHORT	    { datoDeclarado = TIPO_SHORT; }
            | VOID      { datoDeclarado = TIPO_VOID; }
            ;




// EXPRESIONES
// expresion: expAsignacion
//         | expresion ',' expAsignacion
// ;

// expAsignacion: expCondicional
//             | expUnaria operAsignacion expAsignacion 
//             ;

// operAsignacion: '=' 
//               | MAS_IGUAL
// 			  | MENOS_IGUAL
// 			  | POR_IGUAL
//               ;

// expCondicional: expOr
//              | expOr OR expCondicional
//              | expOr '?' expresion ':' expCondicional  
//              ;

// expOr: expAnd
//      | expOr OR expAnd
//      ;

// expAnd: expIgualdad
//       | expAnd AND expIgualdad
//       ;

// expIgualdad: expRelacional
//           | expIgualdad IGUALDAD expRelacional
//           | expIgualdad DESIGUALDAD expRelacional
//           ;

// expRelacional: expAditiva
//             | expRelacional '<' expAditiva
//             | expRelacional MENORIGUAL expAditiva
//             | expRelacional '>' expAditiva
//             | expRelacional MAYORIGUAL expAditiva
//             ;

// expAditiva: expMultiplicativa
//          | expAditiva '+' expMultiplicativa
//          | expAditiva '-' expMultiplicativa
//          ;

// expMultiplicativa: expUnaria
//                 | expMultiplicativa '*' expUnaria
//                 | expMultiplicativa '/' expUnaria
//                 | expMultiplicativa '%' expUnaria
//                 ;

// expUnaria: expPostfijo 
//          | MAS_MAS expUnaria
//          | MENOS_MENOS expUnaria
//          | expUnaria MAS_MAS
//          | expUnaria MENOS_MENOS
//          | operUnario expUnaria
//          | SIZEOF '(' tipoDeDato ')'
//          ;

// operUnario: '&'
//           | '*'
//           | '-'
//           | '!'
//           ;

// expPostfijo: expPrimaria
//           | expPostfijo '[' expresion ']' 
//           | expPostfijo '(' listaArgumentos ')'
//           ;



expAsignacion: IDENTIFICADOR '=' expresion ';'          { validarAsignacion($<tkn.valor>1, $<tkn.es_numero>3, yylineno);}
            | IDENTIFICADOR MAS_IGUAL expresion ';'     { validarAsignacion($<tkn.valor>1, $<tkn.es_numero>3, yylineno);}
			| IDENTIFICADOR MENOS_IGUAL expresion ';'   { validarAsignacion($<tkn.valor>1, $<tkn.es_numero>3, yylineno);}
 			| IDENTIFICADOR POR_IGUAL expresion ';'     { validarAsignacion($<tkn.valor>1, $<tkn.es_numero>3, yylineno);}

expresion: expresion OR expresion 		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion AND expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion MAYORIGUAL expresion    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion '>' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion MENORIGUAL expresion	{ if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion '<' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion DESIGUALDAD expresion 	{ if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
		| expresion '+' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }   
		| expresion '-' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }  
		| expresion '*' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }  
		| expresion '/' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }   
		| expresion '^' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }   
		| expresion '%' expresion		    { if (!$<tkn.es_numero>1 || !$<tkn.es_numero>3) agregarError("Error en la expresion","Semantico", yylineno); else $<tkn.es_numero>$ = 1; }
        | expPrimaria                       {$<tkn.es_numero>$ = $<tkn.es_numero>1;}
;

expPrimaria: CONST_DEC                              {$<tkn.es_numero>$ = 1;}
           | CONST_OCT                              {$<tkn.es_numero>$ = 1;}
           | CONST_HEX                              {$<tkn.es_numero>$ = 1;}
           | CONST_REAL                             {$<tkn.es_numero>$ = 1;}
           | CONST_CARACTER                         {$<tkn.es_numero>$ = 1;}
           | LITERAL_CADENA                         {$<tkn.es_numero>$ = 0;}
           | IDENTIFICADOR '(' listaArgumentos ')'  {validarArgumentos($<tkn.valor>1, yylineno); LArgs = NULL; }
           | IDENTIFICADOR                          {if (buscarPorId($<tkn.valor>1)) { $<tkn.es_numero>$ = 1; $<tkn.es_id>$ = 1;} else {$<tkn.es_numero>$ = 1; agregarError("Identificador no reconocido", "Semantico", yylineno);}}
           | '(' expresion ')'                      {$<tkn.es_numero>$ = $<tkn.es_numero>2;}
           ;

listaArgumentos: expresion                    {agregarArgumento($<tkn.valor>1, $<tkn.es_id>1); }
              | listaArgumentos ',' expresion {agregarArgumento($<tkn.valor>3, $<tkn.es_id>3); }
              | /* vacio */
              ;





caracterDeCorte:	';' | '\n'


%%

int yyerror(const char *msg) {
    // agregarNoReconocido(0, yylineno);
    agregarError(msg, "Sintactico", yylineno);
    return 1;
}
