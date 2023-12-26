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
    CARACTERDEPUNTUACION = 264,
    CHAR = 265,
    DOUBLE = 266,
    FLOAT = 267,
    INT = 268,
    LONG = 269,
    SHORT = 270,
    VOID = 271,
    SIGNED = 272,
    UNSIGNED = 273,
    IF = 274,
    ELSE = 275,
    WHILE = 276,
    DO = 277,
    SWITCH = 278,
    FOR = 279,
    CASE = 280,
    DEFAULT = 281,
    PALABRARESERVADA = 282,
    BREAK = 283,
    RETURN = 284,
    GOTO = 285,
    CONTINUE = 286,
    COMENTARIO = 287,
    IDENTIFICADOR = 288,
    SIZEOF = 289,
    MENOS_MENOS = 290,
    MAS_MAS = 291,
    MAS_IGUAL = 292,
    MENOS_IGUAL = 293,
    POR_IGUAL = 294,
    OR = 295,
    AND = 296,
    MAYORIGUAL = 297,
    MENORIGUAL = 298,
    DESIGUALDAD = 299,
    IGUALDAD = 300
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 25 "src/scanner.y"
 

    char valor[100];

#line 108 "obj/scanner.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_OBJ_SCANNER_TAB_H_INCLUDED  */
