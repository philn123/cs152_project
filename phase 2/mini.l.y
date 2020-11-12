%{
 #include <stdio.h>
 #include <stdlib.h>

 #include "string.h"
 #include "y.tab.h"

 void yyerror(const char *msg);
 extern int currLine;
 extern int currPos;
 FILE * yyin;
%}

%union{
    char* cval;
    int ival;
}

%token FUNCTION IDENTIFIER SEMICOLON BEGINPARAMS ENDPARAMS BEGINLOCALS ENDLOCALS BEGINBODY ENDBODY

%type<cval> IDENTIFIER

%error-verbose
%start program


%% 

program:    /* epsilon */ {printf("program -> epsilon\n");}
        |   program function {printf("program -> program function\n");}
        ;

function:   FUNCTION IDENTIFIER SEMICOLON 
            BEGINPARAMS dec_loop ENDPARAMS 
            BEGINLOCALS dec_loop ENDLOCALS
            BEGINBODY statement_loop ENDBODY
            {
               printf("function -> FUNCTION IDENTIFIER SEMICOLON ");
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