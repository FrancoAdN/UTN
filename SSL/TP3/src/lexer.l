%{ 
	#include <stdio.h>
	#include <string.h>
	#include "scanner.tab.h"
	#include "listas.h"
%}

%option noyywrap
%option yylineno


OP_DIVISION                 "/"
OP_MULTIPLICACION           "*"
INICIO_COMENTARIO_SIMPLE    "//"


/*Seccion Tokens*/
DIGITODECIMAL               [1-9]
DIGITOOCTAL                 [0-7]
DIGITOHEXADECIMAL           [0-9a-fA-F]
DIGITO                      [0-9]

LETRAS                      [a-zA-Z]
IDENTIFICADOR               [a-zA-Z_]+[a-zA-Z0-9_]*
COMILLAS_SIMPLE             "\'"
COMILLAS_DOBLES             "\""


PR_CHAR                     "char"
PR_INT                      "int"
PR_SHORT                    "short"
PR_FLOAT                    "float"
PR_UNSIGNED                 "unsigned"
PR_CONST                    "const"
PR_SIGNED                   "signed"
PR_LONG                     "long"
PR_DOUBLE                   "double"
PR_ENUM                     "enum"
PR_TYPEDEF                  "typedef"
PR_VOLATILE                 "volatile"



/*Palabra Reservada - Estructuras de control*/
PR_IF                       "if"
PR_ELSE                     "else"
PR_FOR                      "for"
PR_WHILE                    "while"
PR_DO                       "do"
PR_SWITCH                   "switch"
PR_CASE                     "case"
PR_BREAK                    "break"
PR_CONTINUE                 "continue"
PR_GOTO                     "goto"
PR_DEFAULT                  "default"

/*Palabra Reservada - Otros*/
PR_AUTO                     "auto"
PR_REGISTER                 "register"
PR_STATIC                   "static"
PR_EXTERN                   "extern"
PR_SIZEOF                   "sizeof"
PR_UNION                    "union"
PR_STRUCT                   "struct"
PR_VOID                     "void"
PR_RETURN					"return"

/*Palabra Reservada - Otros*/
PR_OTROS   {PR_AUTO}|{PR_CONST}|{PR_ENUM}|{PR_EXTERN}|{PR_REGISTER}|{PR_STATIC}|{PR_STRUCT}|{PR_TYPEDEF}|{PR_UNION}|{PR_VOLATILE}


%%

"{"		{ return '{'; }
"}"		{ return '}'; }
"="		{ return '='; }
"<"		{ return '<'; }
">"		{ return '>'; }
"+"		{ return '+'; }
"-"		{ return '-'; }
"*"		{ return '*'; }
"&"		{ return '&'; }
"/"		{ return '/'; }
"%"		{ return '%'; }
";"		{ return ';'; }
":"		{ return ':'; }
"("		{ return '('; }
")"		{ return ')'; }
","		{ return ','; }
"["		{ return '['; }
"]"		{ return ']'; }
"?"		{ return '?'; }
"!"		{ return '!';}


"||"	{ return OR; }
"&&"	{ return AND; }
"=="	{ return IGUALDAD; }
">="	{ return MAYORIGUAL; }
"<="	{ return MENORIGUAL; }
"!=" 	{ return DESIGUALDAD; }
"+=" 	{ return MAS_IGUAL; }
"-=" 	{ return MENOS_IGUAL; }
"*=" 	{ return POR_IGUAL; }
"++"	{ return MAS_MAS; }
"++"	{ return MENOS_MENOS; }

{PR_WHILE} 		{ return WHILE; }
{PR_IF}		    { return IF; }
{PR_ELSE}		{ return ELSE; }
{PR_DO}		    { return DO; }
{PR_SWITCH}		{ return SWITCH; }
{PR_FOR}		{ return FOR; }
{PR_CASE}		{ return CASE; }
{PR_BREAK}		{ return BREAK; }
{PR_DEFAULT}	{ return DEFAULT; }
{PR_CHAR}		{ strcpy(yylval.valor, yytext);return CHAR; }
{PR_DOUBLE}		{ strcpy(yylval.valor, yytext);return DOUBLE; }
{PR_FLOAT}		{ strcpy(yylval.valor, yytext);return FLOAT; }
{PR_INT}		{ strcpy(yylval.valor, yytext); return INT; }
{PR_LONG}		{ return LONG; }
{PR_SHORT}		{ strcpy(yylval.valor, yytext);return SHORT; }
{PR_VOID}		{ strcpy(yylval.valor, yytext);return VOID; }
{PR_SIZEOF}		{ return SIZEOF; }
{PR_RETURN}		{ return RETURN; }
{PR_SIGNED}    	{ return SIGNED;}
{PR_UNSIGNED} 	{ return UNSIGNED;}
{PR_CONTINUE} 	{ return CONTINUE;}
{PR_GOTO}		{ return GOTO;}



{DIGITODECIMAL}{DIGITO}*                            { strcpy(yylval.valor, yytext); return CONST_DEC; }
0{DIGITOOCTAL}*                                     { strcpy(yylval.valor, yytext); return CONST_OCT; }
0[xX]{DIGITOHEXADECIMAL}+                           { strcpy(yylval.valor, yytext); return CONST_HEX; }
(-?){DIGITO}*(\.){DIGITO}+                          { strcpy(yylval.valor, yytext); return CONST_REAL; }
{COMILLAS_SIMPLE}{LETRAS}{COMILLAS_SIMPLE}          { strcpy(yylval.valor, yytext); return CONST_CARACTER; }
{COMILLAS_DOBLES}(.*){COMILLAS_DOBLES}              { strcpy(yylval.valor, yytext); return LITERAL_CADENA;}



{PR_OTROS} { return PALABRARESERVADA; } 

{IDENTIFICADOR}  { strcpy(yylval.valor, yytext); return IDENTIFICADOR; }

{INICIO_COMENTARIO_SIMPLE}{1,10}(.*)                                                                        { return COMENTARIO;}
{OP_DIVISION}{OP_MULTIPLICACION}+(.*)[ \r\t\n]*(.*)[ \r\t\n]*(.*){OP_MULTIPLICACION}+{OP_DIVISION}          { return COMENTARIO;}


\n	{return '\n';}
[\t ]

.				{ agregarNoReconocido(1, yylineno); }





%%

int main(int argc, char* argv[]) {

	yyin = fopen(argv[1], "r");
	yyparse();

	reporteVariableDeclaradas();
	reporteSentencias();
	reporteNoReconocido();
	reporteFunciones();
	return 0;
}