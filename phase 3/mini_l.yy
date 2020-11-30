%{
    /* TODO:Need to remove these later */
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

      struct var_type
      {
         string code;
         string index;
         bool isAnArray;
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

%token END 0 "end of file";

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

%type <string> functions function ident statement_loop comp
%type <dec_type> dec_loop declaration
%type <list<string>> id_loop

 /* Data type may change */
%type <string> statement A expression multiplicative_expr term term_top
%type <var_type> var
%% 

prog_start: functions {cout << $1 << endl;}
	  ;

functions: /* epsilon */ {$$ = "";}
	 | functions function	{$$ = $1 + "\n" + $2;}
	 ;

function:   FUNCTION ident SEMICOLON 
            BEGINPARAMS dec_loop ENDPARAMS 
            BEGINLOCALS dec_loop ENDLOCALS
            BEGINBODY statement_loop ENDBODY
            {
               $$ = "func " + $2 + "\n"; //adding func name
               $$ += $5.code;

               int i = 0;
               for (list<string>::iterator it = $5.ids.begin(); it != $5.ids.end(); ++it)
               {
                  $$ += *it + " $" + to_string(i) + "\n";
                  ++i;
               }

               $$ += $8.code;
               $$ += $11;
               $$ += "endfunc";
            }
            ;
         
dec_loop:    /* epsilon */ {$$.code = ""; $$.ids = list<string>(); }//empty code & ids
        |   declaration SEMICOLON dec_loop   
            {
               $$.code = $1.code + "\n" + $3.code;
               $$.ids = $1.ids;

               for (list<string>::iterator it = $3.ids.begin(); it != $3.ids.end(); ++it)
               {
                  $$.ids.push_back(*it);
               }
            }
        |   declaration error dec_loop{yy::parser::error(@2, "Syntax error, missing semicolon in declaration."); yyerrok;}
        ;

statement_loop:   statement SEMICOLON {$$ = $1;}
               | statement_loop statement SEMICOLON {$$ = $1 + "\n" + $2 + "\n";}
               | statement error {yyerrok; yyerror("Syntax error, missing semicolon in statement.");}
               | statement_loop statement error {yyerrok; yyerror("Syntax error, missing semicolon in statement.");}
               ;
            

declaration:   id_loop COLON INTEGER
               {
                  for(list<string>::iterator it = $1.begin(); it != $1.end(); ++it)
                  {
                     $$.code += ". " + *it + "\n";
                     $$.ids.push_back(*it);
                  }
               }
            |  id_loop COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
               {
                  for(list<string>::iterator it = $1.begin(); it != $1.end(); ++it)
                  {
                     $$.code += ".[] " + *it + ", " + to_string($5) + "\n";
                     $$.ids.push_back(*it);
                  }
               }
            
            |  id_loop COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
              {printf("id_loop -> id_loop COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", $5, $8);}
               
            |  id_loop error INTEGER {yyerrok; yyerror("Syntax error, invalid declaration, missing colon.");}
            |  id_loop error ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {yyerrok; yyerror("Syntax error, invalid declaration, missing colon.");}
            |  id_loop error ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER 
               {yyerrok; yyerror("Syntax error, invalid declaration, missing colon.");}
               ;
            
id_loop: ident {$$.push_back($1);}
      | id_loop COMMA ident {$$ = $1; $$.push_back($3);}
      | id_loop error ident {yy::parser::error(@2, "Syntax error, missing semicolon in declaration."); yyerrok;}
      ;

statement: A {$$ = $1;}
         | B {printf("statement -> B\n");}
         | C {printf("statement -> C\n");}
         | D {printf("statement -> D\n");}
         | E {printf("statement -> E\n");}
         | F {printf("statement -> F\n");}
         | G {printf("statement -> G\n");}
         | H {printf("statement -> H\n");}
         | I {printf("statement -> I\n");}
         ;

A: var ASSIGN expression 
   {
      if ($1.isAnArray)
      {
         $$ = "[]= " + $1.code + ", " + $1.index + ", " + $3 + "\n";
      }
      else 
      {
         $$ = "= " + $1.code + ", " + $3;
      }
   }
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

comp: EQ {$$ = "==";}
   |  NEQ {$$ = "!=";}
   |  LT {$$ = "<";}
   |  GT {$$ = ">";}
   |  LTE {$$ = "<=";}
   |  GTE {$$ = ">=";}
   ;

expression: multiplicative_expr {$$ = $1;}
         | multiplicative_expr ADD expression {printf("expression ->  multiplicative_expr ADD expression\n");}
         | multiplicative_expr SUB expression {printf("expression ->  multiplicative_expr SUB expression\n");}
         ;

multiplicative_expr: term {$$ = $1;}
                  | term MULT multiplicative_expr {printf("multiplicative_expr -> term MULT multiplicative_expr\n");}
                  | term DIV multiplicative_expr {printf("multiplicative_expr -> term DIV multiplicative_expr\n");}
                  | term MOD multiplicative_expr {printf("multiplicative_expr -> term MOD multiplicative_expr\n");}
                  ;

term: term_top {$$ = $1;}
   |  SUB term_top %prec UMINUS {printf("term -> SUB term_top\n");}
   |  ident term_expression {printf("term -> ident term_expression\n");}
   ;
term_top: var {$$ = $1.code;}
      |  NUMBER {$$ = to_string($1);}
      |  L_PAREN expression R_PAREN {printf("term_top -> L_PAREN expression R_PAREN\n");}
      ;
term_expression: L_PAREN term_exp R_PAREN {printf("term_expression -> L_PAREN term_exp R_PAREN\n");}
               | L_PAREN R_PAREN {printf("term_expression -> L_PAREN R_PAREN\n");}
               ;
term_exp:   expression {printf("term_exp -> expression\n");}
         |  expression COMMA term_exp {printf("term_exp -> expression COMMA term_exp");}
         |  expression error term_exp {yyerrok; yyerror("Syntax error, missing comma inbetween expressions.");}
         ;

var:  ident {$$.code = "_" + $1; $$.index = ""; $$.isAnArray = false;}
   |  ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET 
      {
         $$.code = "_" + $1;
         $$.index = $3;
         $$.isAnArray = true;
      }
   |  ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
      {
         printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");
      }
   ;

ident:   IDENT {$$ = $1;}
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

void yyerror(const char *msg)
{
    cout << "-----ERROR-----" << endl;
}

