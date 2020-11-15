%{
 #include <stdio.h>
 #include <stdlib.h>

 int yylex();
 void yyerror(const char *msg);
 extern int currLine;
 extern int currPos;
 FILE * yyin;
%}

%union{
    char* cval;
    int ival;
}

%define parse.error verbose
%start program

%token FUNCTION BEGINPARAMS ENDPARAMS BEGINLOCALS ENDLOCALS BEGINBODY ENDBODY
%token INTEGER ARRAY OF IF THEN ENDIF ELSE WHILE DO FOR BEGINLOOP ENDLOOP CONTINUE
%token READ WRITE AND OR NOT TRUE FALSE RETURN
%token SUB ADD MULT DIV MOD
%token EQ NEQ LT GT LTE GTE
%token SEMICOLON COLON COMMA L_PAREN R_PAREN L_SQUARE_BRACKET R_SQUARE_BRACKET ASSIGN
%token IDENT NUMBER
%type<cval> IDENT
%type<ival> NUMBER

%% 

program:    /* epsilon */ {printf("program -> epsilon\n");}
        |   program function {printf("program -> program function\n");}
        ;

function:   FUNCTION IDENT SEMICOLON 
            BEGINPARAMS dec_loop ENDPARAMS 
            BEGINLOCALS dec_loop ENDLOCALS
            BEGINBODY statement_loop ENDBODY
            {
               printf("function -> FUNCTION IDENT SEMICOLON ");
               printf("BEGINPARAMS dec_loop ENDPARAMS ");
               printf("BEGINLOCALS dec_loop ENDLOCALS ");
               printf("BEGINBODY statement_loop ENDBODY ");
            }
            ;
         
dec_loop:    /* epsilon */ {printf("dec_loop -> epsilon\n");}
        |   dec_loop declaration SEMICOLON {printf("dec_loop -> dec_loop declaration SEMICOLON\n");}
        ;

statement_loop:   statement SEMICOLON {printf("statement_loop -> statement SEMICOLON\n");}
               | statement_loop statement SEMICOLON {printf("statement_loop -> statement_loop statement SEMICOLON\n");}
               ;
            

declaration:   id_loop COLON assignment {printf("declaration -> id_loop COLON assignment\n");}
               ;
            
id_loop: IDENT {printf("id_loop -> IDENT\n");}
      | id_loop COMMA IDENT {printf("id_loop -> id_loop COMMA IDENT\n");}
      ;

assignment: INTEGER {printf("assignment -> INTEGER\n");}
         | ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("assignment -> ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
         | ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER  
                           {printf("assignment -> ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER\n");}
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
      printf("E -> FOR var ASSIGN NUMBER SEMICOLON bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_loop ENDLOOP\n");
   }
   ;

F: READ var var_loop {printf("F -> READ var var_loop\n");}
   ;

G: WRITE var var_loop {printf("G -> WRITE var var_loop\n");}
   ;

var_loop:   /* epsilon */ {printf("var_loop -> epsilon\n");}
         | COMMA var var_loop {printf("var_loop -> COMMA var var_loop\n");}
         ;

H: CONTINUE {printf("H -> CONTINUE\n");};

I: RETURN expression {printf("I -> RETURN expression\n");};

bool_expr: relation_and_expr bool_expr_loop {printf("bool_expr -> relation_and_expr bool_expr_loop\n");}
   ;
bool_expr_loop:   /* epsilon */ {printf("bool_expr_loop -> epsilon\n");}
               | OR relation_and_expr bool_expr_loop {printf("bool_expr_loop -> OR relation_and_expr bool_expr_loop\n");}
               ;

relation_and_expr: relation_expr relation_and_expr_loop {printf("relation_and_expr -> relation_expr relation_and_expr_loop\n");}
   ;
relation_and_expr_loop:   /* epsilon */ {printf("relation_and_expr_loop -> epsilon\n");}
               | AND relation_expr relation_and_expr_loop {printf("relation_and_expr_loop -> AND relation_expr relation_and_expr_loop\n");}
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
               |  ADD multiplicative_expr expression_loop {printf("expression_loop -> ADD multiplicative_expr expression_loop\n");}
               |  SUB multiplicative_expr expression_loop {printf("expression_loop -> SUB multiplicative_expr expression_loop\n");}
               ;

multiplicative_expr: term multi_loop {printf("multiplicative_expr -> term multi_loop\n");}
                  ;
multi_loop: /* epsilon */ {printf("multi_loop -> EPSILON\n");}
         |  MULT term multi_loop {printf("multi_loop -> MULT term multi_loop\n");}
         |  DIV term multi_loop {printf("multi_loop -> DIVterm multi_loop\n");}
         |  MOD term multi_loop {printf("multi_loop -> MOD term multi_loop\n");}
         ;

term: term_top {printf("term -> term_top\n");}
   |  SUB term_top {printf("term -> SUB term_top\n");}
   |  IDENT L_PAREN term_expression R_PAREN {printf("term -> IDENT L_PAREN term_expression R_PAREN\n");}
   ;
term_top: var {printf("term_top -> var\n");}
      |  NUMBER {printf("term_top -> NUMBER\n");}
      |  L_PAREN expression R_PAREN {printf("term_top -> L_PAREN expression R_PAREN\n");}
      ;
term_expression: /* epsilon */ {printf("term_expression -> EPSILON\n");}
               | expression {printf("term_expression -> expression\n");}
               | expression COMMA term_expression {printf("term_expression -> expression COMMA term_expression\n");}
               ;

var:  IDENT {printf("var -> IDENT\n");}
   |  IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET var_bracket {printf("var -> IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET var_bracket\n");}
   ;
var_bracket:   /* epsilon */ {printf("var_bracket -> EPSILON\n");}
            |  L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var_bracket -> L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");}
            ;
%%

int main(int argc, char **argv) {
   if (argc > 1) {
      yyin = fopen(argv[1], "r");
      if (yyin == NULL){
         printf("syntax: %s filename\n", argv[0]);
      }//end if
   }//end if
   yyparse(); // Calls yylex() for tokens.
   return 0;
}

void yyerror(const char *msg) {
   printf("** Line %d, position %d: %s\n", currLine, currPos, msg);
}
