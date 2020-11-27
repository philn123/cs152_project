   /* cs152 phase 1 project */
   /* Phillip Nguyen */

/* Definitions */
%{ 
   #include <iostream>  
   #define YY_DECL yy::parser::symbol_type yylex()
   #include "y.tab.hh"
   static yy::location loc;

   /* TODO: need to remove these when done */ int currLine = 1, currPos = 1;
%}

%option noyywrap

%{
#define YY_USER_ACTION loc.columns(yyleng);
%}

/* Rules */
/* yytext stores token itself */

DIGIT    [0-9]
LETTER   [a-zA-Z]
UNDERSCORE [_]
IDENTIFIER {LETTER}+(({LETTER}|{DIGIT}|{UNDERSCORE})*({LETTER}|{DIGIT})+)*
%%

%{
loc.step();
%}
	/* RESERVERED WORDS */
"function"  {return yy::parser::make_FUNCTION(loc);}
"beginparams" {return yy::parser::make_BEGINPARAMS(loc);}
"endparams"	{return yy::parser::make_ENDPARAMS(loc); }
"beginlocals"	{return yy::parser::make_BEGINLOCALS(loc);}
"endlocals"	{return yy::parser::make_ENDLOCALS(loc);}
"beginbody"	{return yy::parser::make_BEGINBODY(loc);}
"endbody"	{return yy::parser::make_ENDBODY(loc);}
"integer"	{ return yy::parser::make_INTEGER(loc);}
"array"	   {return yy::parser::make_ARRAY(loc);}
"of"	      {return yy::parser::make_OF(loc);}
"if"	      {return yy::parser::make_IF(loc);}
"then"	   {return yy::parser::make_THEN(loc);}
"endif"	   {return yy::parser::make_ENDIF(loc);}
"else"	   {return yy::parser::make_ELSE(loc);}
"while"	   {return yy::parser::make_WHILE(loc);}
"do"	      {return yy::parser::make_DO(loc);}
"for"	      { return yy::parser::make_FOR(loc);}
"beginloop"	{return yy::parser::make_BEGINLOOP(loc);}
"endloop"	{return yy::parser::make_ENDLOOP(loc);}
"continue"	{return yy::parser::make_CONTINUE(loc);}
"read"	   {return yy::parser::make_READ(loc);}
"write"	   {return yy::parser::make_WRITE(loc);}
"and"	      {return yy::parser::make_AND(loc);}
"or"	      {return yy::parser::make_OR(loc);}
"not"	      {return yy::parser::make_NOT(loc);}
"true"	   {return yy::parser::make_TRUE(loc);}
"false"	   {return yy::parser::make_FALSE(loc);}
"return"	   {return yy::parser::make_RETURN(loc);}

	/* Arithmetic Operators */
"-"            {return yy::parser::make_SUB(loc);}
"+"            {return yy::parser::make_ADD(loc);}
"*"            {return yy::parser::make_MULT(loc);}
"/"            {return yy::parser::make_DIV(loc);}
"%"            {return yy::parser::make_MOD(loc);}

	/* Comparison Operators */
"=="            {return yy::parser::make_EQ(loc);}
"<>"            {return yy::parser::make_NEQ(loc);}
"<"             {return yy::parser::make_LT(loc);}
">"             {return yy::parser::make_GT(loc);}
"<="            {return yy::parser::make_LTE(loc);}
">="            {return yy::parser::make_GTE(loc);}

{IDENTIFIER}	{return yy::parser::make_IDENT(yytext,loc);}
{DIGIT}+        {return yy::parser::make_NUMBER(atoi(yytext),loc);}

	/* Other Special Symbols */ 
";"				{return yy::parser::make_SEMICOLON(loc);}
":" 				{return yy::parser::make_COLON(loc);}
"," 				{return yy::parser::make_COMMA(loc); }
"("            {return yy::parser::make_L_PAREN(loc);}
")"            { return yy::parser::make_R_PAREN(loc);}
"["            {return yy::parser::make_L_SQUARE_BRACKET(loc);}
"]"            {return yy::parser::make_R_SQUARE_BRACKET(loc);}
":="				{return yy::parser::make_ASSIGN(loc);}


	/* White Space and Comments */
("##").*\n	   {loc.step(); loc.lines();}
[ \t]+         {/* ignore spaces */ loc.step();}
"\n"           {loc.step(); loc.lines();}

	/* Error 2 */
({DIGIT}|{UNDERSCORE})+{IDENTIFIER}		/*{printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); exit(0);}*/
{IDENTIFIER}({UNDERSCORE})+		/*{printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); exit(0);}*/

	/* make sure this is at the end of the rules, it will catch anything that is not recognized except newlines (characters not in language) */
.  /* ERROR 1 */

<<EOF>> {return yy::parser::make_END(loc);}

%%
