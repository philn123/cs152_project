   /* cs152 phase 1 project */
   /* Phillip Nguyen */

/* Definitions */

%{   
   #include "y.tab.h"
   int currLine = 1, currPos = 1;
%}

/* Rules */
/* yytext stores token itself */

DIGIT    [0-9]
LETTER   [a-zA-Z]
UNDERSCORE [_]
IDENTIFIER {LETTER}+(({LETTER}|{DIGIT}|{UNDERSCORE})*({LETTER}|{DIGIT})+)*
%%
	/* RESERVERED WORDS */
"function"  {currPos += yyleng; return FUNCTION;}
"beginparams" {currPos += yyleng; return BEGINPARAMS;}
"endparams"	{currPos += yyleng; return ENDPARAMS;}
"beginlocals"	{currPos += yyleng; return BEGINLOCALS;}
"endlocals"	{currPos += yyleng; return ENDLOCALS;}
"beginbody"	{currPos += yyleng; return BEGINBODY;}
"endbody"	{currPos += yyleng; return ENDBODY;}
"integer"	{currPos += yyleng; return INTEGER;}
"array"	   {currPos += yyleng; return ARRAY;}
"of"	      {currPos += yyleng; return OF;}
"if"	      {currPos += yyleng; return IF;}
"then"	   {currPos += yyleng; return THEN;}
"endif"	   {currPos += yyleng; return ENDIF;}
"else"	   {currPos += yyleng; return ELSE;}
"while"	   {currPos += yyleng; return WHILE;}
"do"	      {currPos += yyleng; return DO;}
"for"	      {currPos += yyleng; return FOR;}
"beginloop"	{currPos += yyleng; return BEGINLOOP;}
"endloop"	{currPos += yyleng; return ENDLOOP;}
"continue"	{currPos += yyleng; return CONTINUE;}
"read"	   {currPos += yyleng; return READ;}
"write"	   {currPos += yyleng; return WRITE;}
"and"	      {currPos += yyleng; return AND;}
"or"	      {currPos += yyleng; return OR;}
"not"	      {currPos += yyleng; return NOT;}
"true"	   {currPos += yyleng; return TRUE;}
"false"	   {currPos += yyleng; return FALSE;}
"return"	   {currPos += yyleng; return RETURN;}

	/* Arithmetic Operators */
"-"            {currPos += yyleng; return SUB;}
"+"            {currPos += yyleng; return ADD;}
"*"            {currPos += yyleng; return MULT;}
"/"            {currPos += yyleng; return DIV;}
"%"            {currPos += yyleng; return MOD;}

	/* Comparison Operators */
"=="            {currPos += yyleng; return EQ;}
"<>"            {currPos += yyleng; return NEQ;}
"<"             {currPos += yyleng; return LT;}
">"             {currPos += yyleng; return GT;}
"<="            {currPos += yyleng; return LTE;}
">="            {currPos += yyleng; return GTE;}

{IDENTIFIER}	{currPos += yyleng; return IDENT;}
{DIGIT}+       {currPos += yyleng; yylval.ival = atoi(yytext); return NUMBER;}

	/* Other Special Symbols */ 
";"				{currPos += yyleng; return SEMICOLON;}
":" 				{currPos += yyleng; return COLON;}
"," 				{currPos += yyleng; return COMMA;}
"("            {currPos += yyleng; return L_PAREN;}
")"            {currPos += yyleng; return R_PAREN;}
"["            {currPos += yyleng; return L_SQUARE_BRACKET;}
"]"            {currPos += yyleng; return R_SQUARE_BRACKET;}
":="				{currPos += yyleng; return ASSIGN;}


	/* White Space and Comments */
("##").*\n		{currLine++; currPos = 1;}
[ \t]+         {/* ignore spaces */ currPos += yyleng;}
"\n"           {currLine++; currPos = 1;}

	/* Error 2 */
({DIGIT}|{UNDERSCORE})+{IDENTIFIER}		{printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext); exit(0);}
{IDENTIFIER}({UNDERSCORE})+		{printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext); exit(0);}

	/* make sure this is at the end of the rules, it will catch anything that is not recognized except newlines (characters not in language) */
	/* Error 1 */
.              {printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", currLine, currPos, yytext); exit(1);}

%%
