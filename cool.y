/*
 *  cool.y
 *              Parser definition for the COOL language.
 *
 */
%{
#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

extern char *curr_filename;

void yyerror(char *s);        /*  defined below; called for each parse error */
extern int yylex();           /*  the entry point to the lexer  */

/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

Program ast_root;	      /* the result of the parse  */
Classes parse_results;        /* for use in semantic analysis */
int omerrs = 0;               /* number of errors in lexing and parsing */
%}

/* A union of all the types that can be the result of parsing actions. */
%union {
  Boolean boolean;
  Symbol symbol;
  Program program;
  Class_ class_;
  Classes classes;
  Feature feature;
  Features features;
  Formal formal;
  Formals formals;
  Case case_;
  Cases cases;
  Expression expression;
  Expressions expressions;
  char *error_msg;
}

/* 
   Declare the terminals; a few have types for associated lexemes.
   The token ERROR is never used in the parser; thus, it is a parse
   error when the lexer returns it.

   The integer following token declaration is the numeric constant used
   to represent that token internally.  Typically, Bison generates these
   on its own, but we give explicit numbers to prevent version parity
   problems (bison 1.25 and earlier start at 258, later versions -- at
   257)
*/
%token CLASS 258 ELSE 259 FI 260 IF 261 IN 262 
%token INHERITS 263 LET 264 LOOP 265 POOL 266 THEN 267 WHILE 268
%token CASE 269 ESAC 270 OF 271 DARROW 272 NEW 273 ISVOID 274
%token <symbol>  STR_CONST 275 INT_CONST 276 
%token <boolean> BOOL_CONST 277
%token <symbol>  TYPEID 278 OBJECTID 279 
%token ASSIGN 280 NOT 281 LE 282 ERROR 283

/*  DON'T CHANGE ANYTHING ABOVE THIS LINE, OR YOUR PARSER WONT WORK       */
/**************************************************************************/
 
   /* Complete the nonterminal list below, giving a type for the semantic
      value of each non terminal. (See section 3.6 in the bison 
      documentation for details). */

/* Declare types for the grammar's non-terminals. */
%type <program> program
%type <classes> class_list
%type <class_> class
%type <features> dummy_feature_list

/* You will want to change the following line. */
%type <features> features_list
%type <features> more_than_one_feature
%type <feature> feature
%type <formals> formals
%type <formal> formal
%type <formals> dummy_formal_list
%type <expressions> comma_more_expr
%type <expressions> expr
%type <expressions> more_than_one_expr
%type <expresions> let_aux
%type <cases> case_aux
%type <case_> case_aux_2


/* Precedence declarations go here. */

%right ASSIGN
%left NOT
%nonassoc LE '<' '=' 
%left '+' '-'
%left '*' '/'
%left ISVOID
%left '~'
%left '@'
%left '.'


%%
/* 
   Save the root of the abstract syntax tree in a global variable.
*/
program	: class_list	{ ast_root = program($1); }
  ;


class_list
	: class			/* single class */
		{ $$ = single_Classes($1);
                  parse_results = $$; }
	| class_list class	/* several classes */
		{ $$ = append_Classes($1,single_Classes($2)); 
                  parse_results = $$; }
	;

/* If no parent is specified, the class inherits from the Object class. */
class	: CLASS TYPEID '{' dummy_feature_list '}' ';'
		{ $$ = class_($2,idtable.add_string("Object"),$4,
			      stringtable.add_string(curr_filename)); }
	| CLASS TYPEID INHERITS TYPEID '{' dummy_feature_list '}' ';'
		{ $$ = class_($2,$4,$6,stringtable.add_string(curr_filename)); }
  | CLASS error '{' features_list '}' ';' { yyclearin; $$ = NULL; }
  | CLASS error '{' error '}' ';' { yyclearin; $$ = NULL; }
	;


features_list : more_than_one_feature { $$ = $1; }
  | dummy_feature_list
  ;
                
more_than_one_feature : error ';' { yyclearin; $$ = NULL; }
  | more_than_one_feature feature ';' { $$ = append_Features($1, single_Features($2)); }
  | feature ';' { $$ = single_Features($1); }
  ;

feature : OBJECTID '(' formals ')' ':' TYPEID '{' expr '}' { $$ = method($1, $3, $6, $8); }
  | OBJECTID ':' TYPEID { $$ = attr($1, $3, no_expr()); }
  | OBJECTID ':' TYPEID ASSIGN expr { $$ = attr($1, $3, $5); }
  ;
                    
/* Feature list may be empty, but no empty features in list. */
dummy_feature_list : /* empty */
  {  $$ = nil_Features(); }
  ;


formals: formal { $$ = single_Formals($1); }
  | formals ',' formal { $$ = append_Formals($1, single_Formals($3)); }
  | dummy_formal_list
  ;

formal : OBJECTID ':' TYPEID { $$ = formal($1, $3); }
  ;

dummy_formal_list : { $$ = nil_Formals(); }
  ;

/* ASSIGN equivale a '<-', que referencia o operador de atribuição */
expr :  OBJECTID ASSIGN expr { $$ = assign($1, $3); }
    
    /* comme_more_expr corresponde à um método auxiliar definido mais abaixo */
    | expr '@' TYPEID '.' OBJECTID '(' comma_more_expr ')' { $$ = static_dispatch($1, $3, $5, $7); }

    | expr '.' OBJECTID '(' comma_more_expr ')' { $$ = dispatch($1, $3, $5); } 
  
    | OBJECTID '(' comma_more_expr ')' { $$ =  dispatch(object(idtable.add_string("self")), $1, $3); }

    | IF expr THEN expr ELSE expr FI { $$ = cond($2, $4, $6); }
    
    | WHILE expr LOOP expr POOL { $$ = loop($2, $4); }

    /* more_than_one_expr corresponde à outro método auxiliar definido mais abaixo */
    | '{' more_than_one_expr '}' { $$ = block($2); }

    /* let_aux método */
    | LET let_aux IN expr { $$ = $2; }

    /* case_aux corresponde à outro método auxiliar definido mais abaixo */
    | CASE expr OF case_aux ESAC { $$ = typcase($2, $4); }

    | NEW TYPEID { $$ = new($2); }

    | ISVOID expr { $$ = isvoid($2); }

    | expr '+' expr { $$ = plus($1, $3); }
    
    | expr '-' expr { $$ = sub($1, $3); }
    
    | expr '*' expr { $$ = mul($1, $3); }
    
    | expr '/' expr { $$ = divide($1, $3); }

    | '~' expr { $$ = neg($2); }

    | expr '<' expr { $$ = lt($1, $3); }
    
    | expr LE expr { $$ = leq($1, $3); }
    
    | expr '=' expr { $$ = equal($1, $3); }
    
    | NOT expr { $$ = comp($2); }

    | '(' expr ')'  { $$ = $2; }
        
    | OBJECTID { $$ = object($1); }
    
    | INT_CONST { $$ = int_const($1); }
    
    | STR_CONST { $$ = string_const($1); }
    
    | BOOL_CONST { $$ = bool_const($1); }


comma_more_expr : { $$ = nil_Expressions(); }
  | expr { $$ = single_Expressions($1); }
  | comma_more_exp ',' expr { $$ = append_Expressions($1, single_Expressions($3)); };


more_than_one_expr : expr ';' { $$ = single_Expressions($1); }
  | more_than_one_expr ';' { $$ = append_Expressions($1, single_Expressions($3)); }
  | error ';' {yyclearin; $$ = NULL }
;


let_aux : OBJECTID ':' TYPEID IN expr  { $$  = let($1, $3, no_expr(), $5)}
  | OBJECTID ':' TYPEID ASSIGN expr IN expr { $$ = let($1, $3, $5, $7) }
  | OBJECTID ':' TYPEID ',' let_aux { $$ = let($1, $3, no_expr(), $5); }
  | OBJECTID ':' TYPEID ASSIGN expr ',' let_aux { $$ = let($1, $3, $5, $7); }
  | error IN expr { yyclearin; $$ =  NULL; }
;


case_aux : case_aux_2 { $$ = single_Cases($1); }
  | case_aux case_aux_2 { $$ = append_Cases($1, single_Cases($2)); }
;


case_aux_2  : OBJECTID ':' TYPEID DARROW expr ';' { $$ = branch($1, $3, $5); }
;



/* end of grammar */
%%

/* This function is called automatically when Bison detects a parse error. */
void yyerror(char *s)
{
  extern int curr_lineno;

  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>50) {fprintf(stdout, "More than 50 errors\n"); exit(1);}
}

