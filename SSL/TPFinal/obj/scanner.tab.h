/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

#ifndef YY_YY_OBJ_SCANNER_TAB_H_INCLUDED
# define YY_YY_OBJ_SCANNER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    LITERAL_CADENA = 258,
    CONST_CARACTER = 259,
    CONST_REAL = 260,
    CONST_HEX = 261,
    CONST_OCT = 262,
    CONST_DEC = 263,
    CHAR = 264,
    DOUBLE = 265,
    FLOAT = 266,
    INT = 267,
    LONG = 268,
    SHORT = 269,
    VOID = 270,
    SIGNED = 271,
    UNSIGNED = 272,
    IF = 273,
    ELSE = 274,
    WHILE = 275,
    DO = 276,
    SWITCH = 277,
    FOR = 278,
    CASE = 279,
    DEFAULT = 280,
    PALABRARESERVADA = 281,
    BREAK = 282,
    RETURN = 283,
    GOTO = 284,
    CONTINUE = 285,
    COMENTARIO = 286,
    IDENTIFICADOR = 287,
    SIZEOF = 288,
    MENOS_MENOS = 289,
    MAS_MAS = 290,
    MAS_IGUAL = 291,
    MENOS_IGUAL = 292,
    POR_IGUAL = 293,
    OR = 294,
    AND = 295,
    MAYORIGUAL = 296,
    MENORIGUAL = 297,
    DESIGUALDAD = 298,
    IGUALDAD = 299
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 24 "src/scanner.y"
 
    struct {
        char valor[100];
        short es_numero;
        short es_id;
    } tkn;


#line 111 "obj/scanner.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_OBJ_SCANNER_TAB_H_INCLUDED  */
