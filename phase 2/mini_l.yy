%{
 extern const char* yytext;
 extern int currLine;
 extern int currPos;
%}

%skeleton "lalr1.cc"
%require "3.0.4"
%defines
%define api.token.constructor
%define api.value.type variant
%define parse.error verbose
%locations

%code requires
{
    #include <stdio.h>
    #include <stdlib.h>
    #include <iostream>
    #include <list>
    #include <string>
    #include <functional>
    using namespace std;
      /* define the sturctures using as types for non-terminals */
      struct dec_type{
         string code;
         list<string> ids;
      };
      /* end the structures for non-terminal types */
}

%code
{
#include "y.tab.hh"
struct tests
{
	string name;
	yy::location loc;
};

	/* you may need these header files 
	 * add more header file if you need more
	 */
#include <sstream>
#include <map>
#include <regex>
#include <set>
yy::parser::symbol_type yylex();
void yyerror(const char *msg);		/*declaration given by TA*/

	/* define your symbol table, global variables,
	 * list of keywords or any function you may need here */
	
	/* end of your code */
}

%token<string> IDENT
%token<int> NUMBER
%token FUNCTION BEGINPARAMS ENDPARAMS BEGINLOCALS ENDLOCALS BEGINBODY ENDBODY
%token INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE
%token READ WRITE AND OR TRUE FALSE RETURN
%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET

%right ASSIGN
%left OR
%left AND
%right NOT
%left LT LTE GT GTE EQ NEQ 
%left ADD SUB
%left MULT DIV MOD 
%right UMINUS
%left L_SQUARE_BRACKET R_SQUARE_BRACKET
%left L_PAREN R_PAREN

%start prog_start

%type <string> functions

%% 

prog_start: functions {cout << $1 << endl;}
	  ;

functions: /* epsilon */ {$$ = "";}
	 | functions function	{printf("functions -> function functions\n");}
	 ;

function:   FUNCTION ident SEMICOLON 
            BEGINPARAMS dec_loop ENDPARAMS 
            BEGINLOCALS dec_loop ENDLOCALS
            BEGINBODY statement_loop ENDBODY
            {
               printf("function -> FUNCTION ident SEMICOLON ");
               printf("BEGINPARAMS dec_loop ENDPARAMS ");
               printf("BEGINLOCALS dec_loop ENDLOCALS ");
               printf("BEGINBODY statement_loop ENDBODY\n");
            }
            ;
         
dec_loop:    /* epsilon */ {printf("dec_loop -> epsilon\n");}
        |   declaration SEMICOLON dec_loop{printf("dec_loop -> declaration SEMICOLON dec_loop\n");}
        |   declaration error dec_loop{yyerrok; yyerror("Syntax error, missing semicolon in declaration.");}
        ;

statement_loop:   statement SEMICOLON {printf("statement_loop -> statement SEMICOLON\n");}
               | statement error {yyerrok; yyerror("Syntax error, missing semicolon in statement.");}
               | statement_loop statement SEMICOLON {printf("statement_loop -> statement_loop statement SEMICOLON\n");}
               | statement_loop statement error {yyerrok; yyerror("Syntax error, missing semicolon in statement.");}
               ;
            

declaration:   id_loop COLON assignment {printf("declaration -> id_loop COLON assignment\n");}
            |  id_loop error assignment {yyerrok; yyerror("Syntax error, invalid declaration, missing colon.");}
               ;
            
id_loop: ident {printf("id_loop -> ident\n");}
      | id_loop COMMA ident {printf("id_loop -> id_loop COMMA ident\n");}
      | id_loop error ident {yyerrok; yyerror("Syntax error, missing comma inbetween identifiers.");}
      ;

assignment: INTEGER {printf("assignment -> INTEGER\n");}
         | ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("assignment -> ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", $3);}
         | ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER  
                           {printf("assignment -> ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", $3, $6);}
         ;

statement: A {printf("statement -> A\n");}
         | B {printf("statement -> B\n");}
         | C {printf("statement -> C\n");}
         | D {printf("statement -> D\n");}
         | E {printf("statement -> E\n");}
         | F {printf("statement -> F\n");}
         | G {printf("statement -> G\n");}
         | H {printf("statement -> H\n");}
         | I {printf("statement -> I\n");}
         ;

A: var ASSIGN expression {printf("A -> var ASSIGN expression\n");}
   | var error expression {yyerrok; yyerror("Syntax error, \":=\" expected.");}
   ;

B: IF bool_expr THEN statement_loop ENDIF {printf("B -> IF bool_expr THEN statement_loop ENDIF\n");}
   | IF bool_expr THEN statement_loop ELSE statement_loop ENDIF {printf("B -> IF bool_expr THEN statement_loop ELSE statement_loop ENDIF\n");}
   ;

C: WHILE bool_expr BEGINLOOP statement_loop ENDLOOP {printf("C -> WHILE bool_expr BEGINLOOP statement_loop ENDLOOP\n");}
   ;

D: DO BEGINLOOP statement_loop ENDLOOP WHILE bool_expr {printf("D -> DO BEGINLOOP statement_loop ENDLOOP WHILE bool_expr");}
   ;

E: FOR var ASSIGN NUMBER SEMICOLON bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_loop ENDLOOP 
   {
      printf("E -> FOR var ASSIGN NUMBER %d SEMICOLON bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_loop ENDLOOP\n", $4);
   }
   | FOR var ASSIGN NUMBER error bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_loop ENDLOOP 
   {
      {yyerrok; yyerror("Syntax error, missing first comma in for loop");}
   }
   | FOR var ASSIGN NUMBER SEMICOLON bool_expr error var ASSIGN expression BEGINLOOP statement_loop ENDLOOP 
   {
      {yyerrok; yyerror("Syntax error, missing second comma in for loop");}
   }
   ;

F: READ var_loop {printf("F -> READ var_loop\n");}
   ;

G: WRITE var_loop {printf("G -> WRITE var_loop\n");}
   ;

var_loop:  var {printf("var_loop -> var\n");}
         | var COMMA var_loop {printf("var_loop -> var COMMA var_loop \n");}
         | var var_loop {printf("Syntax error at line %d position %d: Missing comma in variable list.\n", currLine, currPos);}
         ;

H: CONTINUE {printf("H -> CONTINUE\n");};

I: RETURN expression {printf("I -> RETURN expression\n");};

bool_expr: relation_and_expr bool_expr_loop {printf("bool_expr -> relation_and_expr bool_expr_loop\n");}
   ;
bool_expr_loop:   /* epsilon */ {printf("bool_expr_loop -> epsilon\n");}
               | bool_expr_loop OR relation_and_expr {printf("bool_expr_loop -> bool_expr_loop OR relation_and_expr\n");}
               ;

relation_and_expr: relation_expr relation_and_expr_loop {printf("relation_and_expr -> relation_expr relation_and_expr_loop\n");}
   ;
relation_and_expr_loop:   /* epsilon */ {printf("relation_and_expr_loop -> epsilon\n");}
               | relation_and_expr_loop AND relation_expr {printf("relation_and_expr_loop -> relation_and_expr_loop AND relation_expr\n");}
               ;

relation_expr: relations {printf("relation_expr -> relations\n");}
            | NOT relations {printf("relation_expr -> NOT relations\n");}
            ;
relations:  expression comp expression {printf("relations -> expression comp expression\n");}
         |  TRUE {printf("relations -> TRUE\n");}
         |  FALSE {printf("relations -> FALSE\n");}
         |  L_PAREN bool_expr R_PAREN {printf("relations -> L_PAREN bool_expr R_PAREN\n");}
         ;

comp: EQ {printf("comp -> EQ\n");}
   |  NEQ {printf("comp -> NEQ\n");}
   |  LT {printf("comp -> LT\n");}
   |  GT {printf("comp -> GT\n");}
   |  LTE {printf("comp -> LTE\n");}
   |  GTE {printf("comp -> GTE\n");}
   ;

expression: multiplicative_expr expression_loop {printf("expression -> multiplicative_expr expression_loop\n");}
         ;
expression_loop:  /* epsilon */ {printf("expression_loop -> EPSILON\n");}
               |  expression_loop ADD multiplicative_expr {printf("expression_loop ->  expression_loop ADD multiplicative_expr\n");}
               |  expression_loop SUB multiplicative_expr {printf("expression_loop -> expression_loop SUB multiplicative_expr\n");}
               ;

multiplicative_expr: term multi_loop {printf("multiplicative_expr -> term multi_loop\n");}
                  ;
multi_loop: /* epsilon */ {printf("multi_loop -> EPSILON\n");}
         |  multi_loop MULT term {printf("multi_loop -> multi_loop MULT term\n");}
         |  multi_loop DIV term {printf("multi_loop -> multi_loop DIV term\n");}
         |  multi_loop MOD term  {printf("multi_loop -> multi_loop MOD term\n");}
         ;

term: term_top {printf("term -> term_top\n");}
   |  SUB term_top %prec UMINUS {printf("term -> SUB term_top\n");}
   |  ident term_expression {printf("term -> ident term_expression\n");}
   ;
term_top: var {printf("term_top -> var\n");}
      |  NUMBER {printf("term_top -> NUMBER %d\n", $1);}
      |  L_PAREN expression R_PAREN {printf("term_top -> L_PAREN expression R_PAREN\n");}
      ;
term_expression: L_PAREN term_exp R_PAREN {printf("term_expression -> L_PAREN term_exp R_PAREN\n");}
               | L_PAREN R_PAREN {printf("term_expression -> L_PAREN R_PAREN\n");}
               ;
term_exp:   expression {printf("term_exp -> expression\n");}
         |  expression COMMA term_exp {printf("term_exp -> expression COMMA term_exp");}
         |  expression error term_exp {yyerrok; yyerror("Syntax error, missing comma inbetween expressions.");}
         ;

var:  ident {printf("var -> ident\n");}
   |  ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
   |  ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
      {
         printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");
      }
   ;

ident:   IDENT {printf("ident -> IDENT %s\n", yytext);}
      ;
%%
int main(int argc, char *argv[])
{
	yy::parser p;
	return p.parse();
}

void yy::parser::error(const yy::location& l, const std::string& m)
{
	std::cerr << l << ": " << m << std::endl;
}

