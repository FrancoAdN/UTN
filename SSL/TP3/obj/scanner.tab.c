/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

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

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 1 "src/scanner.y"
  

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



#line 94 "obj/scanner.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
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

#line 197 "obj/scanner.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_OBJ_SCANNER_TAB_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   493

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  68
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  37
/* YYNRULES -- Number of rules.  */
#define YYNRULES  118
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  196

#define YYUNDEFTOK  2
#define YYMAXUTOK   300


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      55,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    65,     2,     2,     2,    53,    64,     2,
      59,    60,    51,    49,    61,    50,     2,    52,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    63,    58,
      43,    37,    44,    62,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    66,     2,    67,    54,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    56,     2,    57,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    38,    39,    40,    41,    42,    45,    46,    47,
      48
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,    57,    57,    58,    61,    62,    63,    71,    72,    73,
      74,    75,    76,    77,    78,    79,    82,    86,    87,    88,
      91,    92,    93,    96,    99,   100,   104,   105,   106,   113,
     114,   115,   116,   130,   133,   134,   135,   138,   139,   140,
     141,   142,   143,   147,   148,   149,   151,   151,   152,   153,
     154,   159,   162,   163,   164,   166,   170,   171,   172,   173,
     174,   175,   182,   183,   186,   187,   190,   191,   192,   193,
     196,   197,   198,   201,   202,   205,   206,   209,   210,   211,
     214,   215,   216,   217,   218,   221,   222,   223,   226,   227,
     228,   229,   232,   233,   234,   235,   236,   237,   238,   241,
     242,   243,   244,   247,   248,   249,   252,   253,   254,   257,
     258,   259,   260,   261,   262,   263,   264,   269,   269
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "LITERAL_CADENA", "CONST_CARACTER",
  "CONST_REAL", "CONST_HEX", "CONST_OCT", "CONST_DEC",
  "CARACTERDEPUNTUACION", "CHAR", "DOUBLE", "FLOAT", "INT", "LONG",
  "SHORT", "VOID", "SIGNED", "UNSIGNED", "IF", "ELSE", "WHILE", "DO",
  "SWITCH", "FOR", "CASE", "DEFAULT", "PALABRARESERVADA", "BREAK",
  "RETURN", "GOTO", "CONTINUE", "COMENTARIO", "IDENTIFICADOR", "SIZEOF",
  "MENOS_MENOS", "MAS_MAS", "'='", "MAS_IGUAL", "MENOS_IGUAL", "POR_IGUAL",
  "OR", "AND", "'<'", "'>'", "MAYORIGUAL", "MENORIGUAL", "DESIGUALDAD",
  "IGUALDAD", "'+'", "'-'", "'*'", "'/'", "'%'", "'^'", "'\\n'", "'{'",
  "'}'", "';'", "'('", "')'", "','", "'?'", "':'", "'&'", "'!'", "'['",
  "']'", "$accept", "input", "line", "sentencia", "sentCompuesta",
  "listaDeclaraciones", "listaSentencias", "sentExpresion",
  "sentSeleccion", "sentIteracion", "sentSalto", "declaracion",
  "declaracionVarSimples", "tipoDeDato", "listaVarSimples", "unaVarSimple",
  "$@1", "definicionFuncion", "lista_argumentos", "argumento",
  "tipoDeRetorno", "expresion", "expAsignacion", "operAsignacion",
  "expCondicional", "expOr", "expAnd", "expIgualdad", "expRelacional",
  "expAditiva", "expMultiplicativa", "expUnaria", "operUnario",
  "expPostfijo", "listaArgumentos", "expPrimaria", "caracterDeCorte", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,    61,   292,   293,
     294,   295,   296,    60,    62,   297,   298,   299,   300,    43,
      45,    42,    47,    37,    94,    10,   123,   125,    59,    40,
      41,    44,    63,    58,    38,    33,    91,    93
};
# endif

#define YYPACT_NINF (-161)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-51)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -161,   168,  -161,   -15,  -161,  -161,  -161,  -161,  -161,  -161,
    -161,  -161,  -161,  -161,  -161,  -161,  -161,   -45,   -21,   428,
     -10,   -14,    66,    35,    17,  -161,    19,    66,    66,  -161,
    -161,  -161,   233,    66,  -161,  -161,  -161,    50,  -161,  -161,
    -161,  -161,  -161,  -161,  -161,     2,  -161,    76,   -13,  -161,
    -161,   -31,    65,   -26,    21,    42,    44,    83,    66,   -50,
    -161,  -161,  -161,  -161,    66,    66,  -161,    94,    66,  -161,
      -7,    71,  -161,   131,    68,    68,   -15,  -161,  -161,  -161,
    -161,  -161,   298,  -161,   -28,  -161,    63,   -29,    80,    86,
      93,  -161,    66,    66,    66,    66,    66,    66,    66,    66,
      66,    66,    66,    66,    66,    66,    66,  -161,  -161,  -161,
    -161,  -161,  -161,    66,    68,    66,    66,    51,    53,    96,
      -3,  -161,  -161,    97,  -161,   363,  -161,  -161,    66,   131,
      92,  -161,     4,   131,  -161,  -161,    65,    68,    -1,   -26,
      21,    21,    42,    42,    42,    42,    44,    44,    68,    68,
      68,  -161,  -161,    67,   -43,   428,   428,    66,    66,  -161,
    -161,  -161,    95,   125,    99,   100,     6,   -17,  -161,   102,
      66,  -161,    66,  -161,   106,  -161,    72,    -2,  -161,   104,
     131,  -161,  -161,   104,  -161,  -161,   428,   105,    66,  -161,
    -161,  -161,  -161,  -161,    74,  -161
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       2,     0,     1,    15,   115,   114,   113,   112,   111,   110,
      37,    38,    39,    40,    41,    42,    61,     0,     0,     0,
       0,     0,     0,     0,     0,   109,     0,     0,     0,   101,
     100,     4,     0,     0,    99,   102,     3,     6,     7,     8,
       9,    10,    11,    12,    33,     0,    14,     0,     0,    62,
      64,    70,    73,    75,    77,    80,    85,    88,     0,    92,
     103,   118,   117,    36,     0,     0,    13,     0,     0,    30,
       0,     0,    29,     0,    94,    93,     0,    37,    38,    39,
      40,    42,     0,    17,     0,     5,    45,    46,     0,    43,
       0,    23,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    96,    95,    66,
      67,    68,    69,     0,    97,   108,     0,     0,     0,     0,
       0,    31,    32,     0,    20,     0,    12,   116,     0,    54,
       0,    34,     0,    54,    63,    71,    73,    88,     0,    76,
      79,    78,    81,    83,    84,    82,    86,    87,    89,    90,
      91,    65,   106,     0,     0,     0,     0,     0,     0,    98,
      16,    21,    49,     0,     0,    53,     0,    46,    44,     0,
       0,   105,     0,   104,    24,    26,     0,     0,    55,     0,
      54,    50,    47,     0,    72,   107,     0,     0,     0,    35,
      52,    51,    25,    27,     0,    28
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -161,  -161,  -161,   -19,  -160,  -161,  -161,  -161,  -161,  -161,
    -161,   -30,  -161,   -72,    32,     0,  -161,  -161,  -127,  -161,
    -161,   -18,   -79,  -161,   -76,  -161,    84,    70,    52,   -73,
      48,   -16,  -161,  -161,  -161,  -161,  -161
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    36,    37,    38,    82,   125,    39,    40,    41,
      42,    43,    44,    45,    88,    89,   130,    46,   164,   165,
      47,    48,    49,   113,    50,    51,    52,    53,    54,    55,
      56,    57,    58,    59,   153,    60,    63
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      67,   123,    83,    86,    70,    86,   169,   181,   128,   115,
      93,    74,    75,   134,    64,    84,   116,   135,    92,   189,
     128,    96,    97,   191,   173,   142,   143,   144,   145,   -48,
     129,    94,   127,    92,   151,    87,   152,   167,    65,   167,
      61,   -48,   114,    62,    69,    91,   117,   118,    92,    68,
     120,   121,   126,   190,    92,   158,   188,   163,    92,    92,
      92,   163,   170,   124,    98,    99,   100,   101,    71,     4,
       5,     6,     7,     8,     9,    72,   138,   137,    73,   137,
     137,   137,   137,   137,   137,   137,   137,   137,   148,   149,
     150,   102,   103,   185,   184,   104,   105,   106,   154,    25,
      26,    27,    28,   107,   108,    85,   161,    95,   163,    90,
     162,   155,    92,   156,    92,   119,    29,    30,   107,   108,
     109,   110,   111,   112,   -50,    33,   186,   171,   172,   122,
      34,    35,   187,    92,   195,    92,   174,   175,   131,   176,
     177,    77,    78,    79,    80,    14,    81,   132,   140,   141,
     146,   147,   133,   166,   137,   157,    92,   159,   178,   179,
      32,   180,   183,   193,   168,   139,   182,   192,     2,     3,
     194,     4,     5,     6,     7,     8,     9,   136,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,     0,    18,
      19,     0,    20,     0,     0,     0,    21,    22,    23,    24,
       0,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,    30,
       0,     0,     0,    31,    32,     0,     0,    33,     0,     0,
       0,     0,    34,    35,    76,     0,   -19,   -19,   -19,   -19,
     -19,   -19,     0,    77,    78,    79,    80,    14,    81,   -19,
       0,     0,   -19,     0,   -19,   -19,     0,   -19,     0,     0,
       0,   -19,   -19,   -19,   -19,     0,   -19,   -19,   -19,   -19,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   -19,   -19,     0,     0,     0,   -19,   -19,
     -19,     0,   -19,     0,     0,     0,     0,   -19,   -19,     3,
       0,     4,     5,     6,     7,     8,     9,     0,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,     0,    18,
      19,     0,    20,     0,     0,     0,    21,    22,    23,    24,
       0,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,    30,
       0,     0,     0,    66,    32,   -22,     0,    33,     0,     0,
       0,     0,    34,    35,     3,     0,     4,     5,     6,     7,
       8,     9,     0,    10,    11,    12,    13,    14,    15,    16,
       0,     0,    17,     0,    18,    19,     0,    20,     0,     0,
       0,    21,    22,    23,    24,     0,    25,    26,    27,    28,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    29,    30,     0,     0,     0,    66,    32,
     160,     0,    33,     0,     0,     0,     0,    34,    35,     3,
       0,     4,     5,     6,     7,     8,     9,     0,    10,    11,
      12,    13,    14,    15,    16,     0,     0,    17,     0,    18,
      19,     0,    20,     0,     0,     0,    21,    22,    23,    24,
       0,    25,    26,    27,    28,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    29,    30,
       0,     0,     0,    66,    32,     0,     0,    33,     0,     0,
       0,     0,    34,    35
};

static const yytype_int16 yycheck[] =
{
      19,    73,    32,     1,    22,     1,   133,     1,    37,    59,
      41,    27,    28,    92,    59,    33,    66,    93,    61,   179,
      37,    47,    48,   183,    67,    98,    99,   100,   101,    58,
      59,    62,    60,    61,   113,    33,   115,    33,    59,    33,
      55,    58,    58,    58,    58,    58,    64,    65,    61,    59,
      68,    58,    82,   180,    61,    58,    58,   129,    61,    61,
      61,   133,    63,    82,    43,    44,    45,    46,    33,     3,
       4,     5,     6,     7,     8,    58,    94,    93,    59,    95,
      96,    97,    98,    99,   100,   101,   102,   103,   104,   105,
     106,    49,    50,   172,   170,    51,    52,    53,   116,    33,
      34,    35,    36,    35,    36,    55,   125,    42,   180,    33,
     128,    60,    61,    60,    61,    21,    50,    51,    35,    36,
      37,    38,    39,    40,    61,    59,    20,    60,    61,    58,
      64,    65,    60,    61,    60,    61,   155,   156,    58,   157,
     158,    10,    11,    12,    13,    14,    15,    61,    96,    97,
     102,   103,    59,    61,   170,    59,    61,    60,    33,    60,
      56,    61,    60,    58,   132,    95,   166,   186,     0,     1,
     188,     3,     4,     5,     6,     7,     8,    93,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    -1,    24,    -1,    -1,    -1,    28,    29,    30,    31,
      -1,    33,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,     1,    -1,     3,     4,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    -1,    24,    -1,    -1,
      -1,    28,    29,    30,    31,    -1,    33,    34,    35,    36,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,     1,
      -1,     3,     4,     5,     6,     7,     8,    -1,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    -1,    24,    -1,    -1,    -1,    28,    29,    30,    31,
      -1,    33,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    57,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65,     1,    -1,     3,     4,     5,     6,
       7,     8,    -1,    10,    11,    12,    13,    14,    15,    16,
      -1,    -1,    19,    -1,    21,    22,    -1,    24,    -1,    -1,
      -1,    28,    29,    30,    31,    -1,    33,    34,    35,    36,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    50,    51,    -1,    -1,    -1,    55,    56,
      57,    -1,    59,    -1,    -1,    -1,    -1,    64,    65,     1,
      -1,     3,     4,     5,     6,     7,     8,    -1,    10,    11,
      12,    13,    14,    15,    16,    -1,    -1,    19,    -1,    21,
      22,    -1,    24,    -1,    -1,    -1,    28,    29,    30,    31,
      -1,    33,    34,    35,    36,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    50,    51,
      -1,    -1,    -1,    55,    56,    -1,    -1,    59,    -1,    -1,
      -1,    -1,    64,    65
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,    69,     0,     1,     3,     4,     5,     6,     7,     8,
      10,    11,    12,    13,    14,    15,    16,    19,    21,    22,
      24,    28,    29,    30,    31,    33,    34,    35,    36,    50,
      51,    55,    56,    59,    64,    65,    70,    71,    72,    75,
      76,    77,    78,    79,    80,    81,    85,    88,    89,    90,
      92,    93,    94,    95,    96,    97,    98,    99,   100,   101,
     103,    55,    58,   104,    59,    59,    55,    71,    59,    58,
      89,    33,    58,    59,    99,    99,     1,    10,    11,    12,
      13,    15,    73,    79,    89,    55,     1,    33,    82,    83,
      33,    58,    61,    41,    62,    42,    47,    48,    43,    44,
      45,    46,    49,    50,    51,    52,    53,    35,    36,    37,
      38,    39,    40,    91,    99,    59,    66,    89,    89,    21,
      89,    58,    58,    81,    71,    74,    79,    60,    37,    59,
      84,    58,    61,    59,    90,    92,    94,    99,    89,    95,
      96,    96,    97,    97,    97,    97,    98,    98,    99,    99,
      99,    90,    90,   102,    89,    60,    60,    59,    58,    60,
      57,    71,    89,    81,    86,    87,    61,    33,    82,    86,
      63,    60,    61,    67,    71,    71,    89,    89,    33,    60,
      61,     1,    83,    60,    92,    90,    20,    60,    58,    72,
      86,    72,    71,    58,    89,    60
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    68,    69,    69,    70,    70,    70,    71,    71,    71,
      71,    71,    71,    71,    71,    71,    72,    73,    73,    73,
      74,    74,    74,    75,    76,    76,    77,    77,    77,    78,
      78,    78,    78,    79,    80,    80,    80,    81,    81,    81,
      81,    81,    81,    82,    82,    82,    84,    83,    83,    83,
      83,    85,    86,    86,    86,    87,    88,    88,    88,    88,
      88,    88,    89,    89,    90,    90,    91,    91,    91,    91,
      92,    92,    92,    93,    93,    94,    94,    95,    95,    95,
      96,    96,    96,    96,    96,    97,    97,    97,    98,    98,
      98,    98,    99,    99,    99,    99,    99,    99,    99,   100,
     100,   100,   100,   101,   101,   101,   102,   102,   102,   103,
     103,   103,   103,   103,   103,   103,   103,   104,   104
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     2,     0,
       1,     2,     0,     2,     5,     7,     5,     7,     8,     2,
       2,     3,     3,     1,     3,     6,     2,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     0,     4,     1,     3,
       1,     6,     3,     1,     0,     2,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     1,     1,     1,     1,
       1,     3,     5,     1,     3,     1,     3,     1,     3,     3,
       1,     3,     3,     3,     3,     1,     3,     3,     1,     3,
       3,     3,     1,     2,     2,     2,     2,     2,     4,     1,
       1,     1,     1,     1,     4,     4,     1,     3,     0,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     1
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 16:
#line 82 "src/scanner.y"
                                                          { agregarSentencia("Compuesta", yylineno);}
#line 1581 "obj/scanner.tab.c"
    break;

  case 23:
#line 96 "src/scanner.y"
                             { agregarSentencia("Expresion", yylineno);}
#line 1587 "obj/scanner.tab.c"
    break;

  case 24:
#line 99 "src/scanner.y"
                                              { agregarSentencia("Seleccion", yylineno);}
#line 1593 "obj/scanner.tab.c"
    break;

  case 25:
#line 100 "src/scanner.y"
                                                             { agregarSentencia("Seleccion", yylineno);}
#line 1599 "obj/scanner.tab.c"
    break;

  case 26:
#line 104 "src/scanner.y"
                                                 { agregarSentencia("Iteracion (while)", yylineno);}
#line 1605 "obj/scanner.tab.c"
    break;

  case 27:
#line 105 "src/scanner.y"
                                                        { agregarSentencia("Iteracion (do-while)", yylineno);}
#line 1611 "obj/scanner.tab.c"
    break;

  case 28:
#line 106 "src/scanner.y"
                                                                { agregarSentencia("Iteracion (for)", yylineno);}
#line 1617 "obj/scanner.tab.c"
    break;

  case 29:
#line 113 "src/scanner.y"
                        { agregarSentencia("Salto (continue)", yylineno);}
#line 1623 "obj/scanner.tab.c"
    break;

  case 30:
#line 114 "src/scanner.y"
                    { agregarSentencia("Salto (break)", yylineno);}
#line 1629 "obj/scanner.tab.c"
    break;

  case 31:
#line 115 "src/scanner.y"
                               { agregarSentencia("Salto (return)", yylineno);}
#line 1635 "obj/scanner.tab.c"
    break;

  case 32:
#line 116 "src/scanner.y"
                                { agregarSentencia("Salto (goto)", yylineno);}
#line 1641 "obj/scanner.tab.c"
    break;

  case 34:
#line 133 "src/scanner.y"
                                                      {agregarDeclaraciones(datoDeclarado); flag_error = 0;}
#line 1647 "obj/scanner.tab.c"
    break;

  case 35:
#line 134 "src/scanner.y"
                                                                                       { agregarFuncion((yyvsp[-4].valor), (yyvsp[-5].valor), cantArgumentos);cantArgumentos = 0;}
#line 1653 "obj/scanner.tab.c"
    break;

  case 36:
#line 135 "src/scanner.y"
                                                                 { flag_error = 1;}
#line 1659 "obj/scanner.tab.c"
    break;

  case 37:
#line 138 "src/scanner.y"
                        { datoDeclarado = TIPO_CHAR; }
#line 1665 "obj/scanner.tab.c"
    break;

  case 38:
#line 139 "src/scanner.y"
                        { datoDeclarado = TIPO_DOUBLE; }
#line 1671 "obj/scanner.tab.c"
    break;

  case 39:
#line 140 "src/scanner.y"
                        { datoDeclarado = TIPO_FLOAT; }
#line 1677 "obj/scanner.tab.c"
    break;

  case 40:
#line 141 "src/scanner.y"
                        { datoDeclarado = TIPO_INT; }
#line 1683 "obj/scanner.tab.c"
    break;

  case 41:
#line 142 "src/scanner.y"
                        { datoDeclarado = TIPO_LONG; }
#line 1689 "obj/scanner.tab.c"
    break;

  case 42:
#line 143 "src/scanner.y"
                        { datoDeclarado = TIPO_SHORT; }
#line 1695 "obj/scanner.tab.c"
    break;

  case 45:
#line 149 "src/scanner.y"
                        { flag_error = 1; free(ListaIdentificadores); ListaIdentificadores = NULL;}
#line 1701 "obj/scanner.tab.c"
    break;

  case 46:
#line 151 "src/scanner.y"
                            { if (!flag_error) agregarIdentificador((yyvsp[0].valor)); else flag_error = 0;}
#line 1707 "obj/scanner.tab.c"
    break;

  case 48:
#line 152 "src/scanner.y"
                                        { if (!flag_error) agregarIdentificador((yyvsp[0].valor)); else flag_error = 0;}
#line 1713 "obj/scanner.tab.c"
    break;

  case 49:
#line 153 "src/scanner.y"
                                           { if (!flag_error) agregarIdentificador((yyvsp[-2].valor)); else flag_error = 0;}
#line 1719 "obj/scanner.tab.c"
    break;

  case 50:
#line 154 "src/scanner.y"
                                {  flag_error = 1; free(ListaIdentificadores); ListaIdentificadores = NULL;}
#line 1725 "obj/scanner.tab.c"
    break;

  case 51:
#line 159 "src/scanner.y"
                                                                                      { agregarFuncion((yyvsp[-4].valor), (yyvsp[-5].valor), cantArgumentos);cantArgumentos = 0;}
#line 1731 "obj/scanner.tab.c"
    break;

  case 55:
#line 166 "src/scanner.y"
                                    {cantArgumentos++;}
#line 1737 "obj/scanner.tab.c"
    break;


#line 1741 "obj/scanner.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 272 "src/scanner.y"


int yyerror(const char *msg) {
    agregarNoReconocido(0, yylineno);
    return 1;
}
