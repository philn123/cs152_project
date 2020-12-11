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
    #include <sstream>
    #include <regex>
    #include <algorithm>
    using namespace std;
      /* define the sturctures using as types for non-terminals */
      struct dec_type{
         string code;
         list<string> ids;
      };

      struct node
      {
         string code;
         string tempRegName;
         string index;
         bool isAnArray;
         string name;
      };

      struct term_type
      {
         string code;
         string tempRegName;
         string index;
         bool isAnArray;
         string type;
         string name;
      };

      enum type
      {
         INTEGER,
         ARRAY,
         FUNCTION,
         CALL
      };

      /* end the structures for non-terminal types */
      string createTempRegister();
      string createLabel();
      void backpatch(string &, const string &, const string &);
      void replaceNewlines(string &, const string &, const string &);
      int checkContinueLoop(string &);
      int inSymbolTable(string &);
      bool hasMain();
      bool errorTwo();
      void extractFunctionCalls();
}

%code
{
#include "y.tab.hh"

struct tests
{
   string name;
   yy::location loc;
   pair <int, int> dimension;
   type type_var;
};

	/* you may need these header files 
	 * add more header file if you need more
	 */
#include <sstream>
#include <map>
#include <regex>
#include <set>
yy::parser::symbol_type yylex();
void yyerror(const char *msg);
void yyerror(string s);			/*declaration given by TA*/

	/* define your symbol table, global variables,
	 * list of keywords or any function you may need here */
   vector<tests> sym_table;
   bool errorHasOccured = false;
   vector <tests> functionCallsOnly;
   vector<string> functionNames;
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
%type <string> statement A B C D E F G H I
%type <node> var expression multiplicative_expr term bool_expr relation_and_expr relation_expr relations
%type <vector<node*>> var_loop term_exp
%type <term_type> term_top
%% 

prog_start: functions 
   {
      string generated_code = $1;

      // Error 2
      if(errorTwo())
      {
         errorHasOccured = true;
         for(auto item : functionCallsOnly)
         {
            string msg = "function call: \"" + item.name + "\" has not been defined.";
            yy::parser::error(item.loc, msg);
         }
      }

      // Error 3
      if (!hasMain())
      {
         errorHasOccured = true;
         yy::parser::error(@1, "No main function defined");
      }

      if (!errorHasOccured)
      {
         replaceNewlines(generated_code, "\n\n", "\n");
         cout << generated_code << endl;
      }
   }
	  ;

functions: /* epsilon */ {$$ = "";}
	 | functions function	
    {
      $$ = $1 + "\n" + $2;
    }
	 ;

function:   FUNCTION ident SEMICOLON 
            BEGINPARAMS dec_loop ENDPARAMS 
            BEGINLOCALS dec_loop ENDLOCALS
            BEGINBODY statement_loop ENDBODY
            {
               int index = inSymbolTable($2);
               if (index == -1 || sym_table.at(index).type_var != FUNCTION)
               {
                  sym_table.push_back({$2, @2, make_pair(-1,-1), FUNCTION});
                  functionNames.push_back($2);
               }
               else //found it 
               {
                  errorHasOccured = true;
                  yy::parser::error(@2, "function name \"" + $2 + "\" is multiply-defined.");
               }

               $$ = "func " + $2 + "\n"; //adding func name
               $$ += $5.code;

               int i = 0;
               for (list<string>::iterator it = $5.ids.begin(); it != $5.ids.end(); ++it)
               {
                  $$ += "= " + *it + ", $" + to_string(i) + "\n";
                  ++i;
               }

               $$ += $8.code;
               $$ += $11;
               $$ += "endfunc";

               // Error 9
               regex continueReg("continue.+");
               smatch matches;

               for (sregex_iterator it = sregex_iterator($11.begin(), $11.end(), continueReg); 
                  it != sregex_iterator(); it++)
               {
                  matches = *it;
                  errorHasOccured = true;
                  string tempString = matches.str(0);
                  int pos = 8; // length of word continue
                  
                  string msg = "Error line " + tempString.substr(pos) + " : continue not within loop.";
                  yyerror(msg);
               }

               extractFunctionCalls();
               sym_table.clear();
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
        |   declaration error dec_loop{yy::parser::error(@2, "Syntax error, missing semicolon in declaration."); yyerrok; 
            errorHasOccured = true;}
        ;

statement_loop:   statement SEMICOLON {$$ = $1;}
               | statement_loop statement SEMICOLON {$$ = $1 + "\n" + $2;}
               | statement error 
               {yy::parser::error(@1, "Syntax error, missing semicolon in statement."); errorHasOccured = true; yyerrok;}
               | statement_loop statement error 
               {yy::parser::error(@1, "Syntax error, missing semicolon in statement."); errorHasOccured = true; yyerrok;}
               ;
            

declaration:   id_loop COLON INTEGER
               {
                  for(list<string>::iterator it = $1.begin(); it != $1.end(); ++it)
                  {
                     if (inSymbolTable(*it) == -1)
                     {
                        $$.code += ". " + *it + "\n";
                        $$.ids.push_back(*it);
                        sym_table.push_back({*it, @2, make_pair(-1,-1), INTEGER});
                     }
                     else //found it 
                     {
                        errorHasOccured = true;
                        yy::parser::error(@2, "symbol \"" + *it + "\" is multiply-defined.");
                     }
                  }
               }
            |  id_loop COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
               {
                  // Error 8
                  if ($5 <= 0)
                  {
                     errorHasOccured = true;
                     yy::parser::error(@1, "array size is less than 1");
                  }

                  for(list<string>::iterator it = $1.begin(); it != $1.end(); ++it)
                  {
                     if (inSymbolTable(*it) == -1)
                     {
                        $$.code += ".[] " + *it + ", " + to_string($5);
                        $$.ids.push_back(*it);
                        sym_table.push_back({*it, @2, make_pair($5,1), ARRAY});
                     }
                     else
                     {
                        errorHasOccured = true;
                        yy::parser::error(@2, "symbol \"" + *it + "\" is multiply-defined.");
                     }
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
         | B {$$ = $1;}
         | C {$$ = $1;}
         | D {$$ = $1;}
         | E {$$ = $1;}
         | F {$$ = $1;}
         | G {$$ = $1;}
         | H {$$ = $1;}
         | I {$$ = $1;}
         ;

A: var ASSIGN expression 
   {
      if ($1.isAnArray)
      {
         if (!$3.tempRegName.empty())
         {
            $$ = $1.code;
            $$ += $3.code;
            $$ += "[]= " + $1.name + ", " + $1.index + ", " + $3.tempRegName + "\n";
         }
         else
         {
            $$ += "[]= " + $1.code + ", " + $1.index + ", " + $3.code + "\n";
         }
      }
      else if ($3.tempRegName.empty())
      {
         $$ = "= " + $1.code + ", " + $3.code + "\n";
      }
      else
      {
         $$ = $3.code;
         $$ += "= " + $1.code + ", " + $3.tempRegName + "\n";
      }
   }
   | var error expression {yyerrok; yyerror("Syntax error, \":=\" expected.");}
   ;

B: IF bool_expr THEN statement_loop ENDIF
   {
      // following fibonacci.mil output
      string trueLabel = createLabel();
      string falseLabel = createLabel();
      string output = "";

      output += $2.code + "\n";
      output += "?:= " + trueLabel + ", " + $2.tempRegName + "\n";
      output += ":= " + falseLabel + "\n";
      output += ": " + trueLabel + "\n";
      output += $4 + "\n";
      output += ": " + falseLabel + "\n";

      $$ = output;
   }
   | IF bool_expr THEN statement_loop ELSE statement_loop ENDIF 
   {
      // following fibonacci.mil output
      string trueLabel = createLabel();
      string falseLabel = createLabel();
      string output = "";

      output += $2.code + "\n";
      output += "?:= " + trueLabel + ", " + $2.tempRegName + "\n";
      output += $6 + "\n";  //false code here, not at the end
      output += ":= " + falseLabel + "\n";
      output += ": " + trueLabel + "\n";
      output += $4 + "\n";
      output += ": " + falseLabel + "\n";

      $$ = output;

   }
   ;

C: WHILE bool_expr BEGINLOOP statement_loop ENDLOOP 
   {
      //following primes.mil logic alongside lecture
      string labelStart = createLabel();
      string label1 = createLabel();
      string label2 = createLabel();
      string output = "";

      // need to backpatch for continue
      string replacement = ":= " + labelStart; //go to the start of loop again
      backpatch($4, "continue", replacement);

      output += ": " + labelStart + "\n";
      output += $2.code + "\n";
      output += "?:= " + label1 + ", " + $2.tempRegName + "\n";
      output += ":= " + label2 + "\n"; // if false, go to label 2 (which is the end)
      output += ": " + label1 + "\n"; // if true, we go here and execute statement loop
      output += $4 + "\n";
      output += ":= " + labelStart + "\n"; // finished one iteration of loop, check condition again
      output += ": " + label2 + "\n"; // false, move on

      $$ = output;
   }
   ;

D: DO BEGINLOOP statement_loop ENDLOOP WHILE bool_expr
   {
      string labelStart = createLabel();
      string boolLabel = createLabel();
      string label2 = createLabel();
      string output = "";

      // need to backpatch for continue
      string replacement = ":= " + boolLabel; //go to the start of loop again
      backpatch($3, "continue", replacement);

      output += ": " + labelStart + "\n";
      output += $3 + "\n";
      output += ": " + boolLabel + "\n";
      output += $6.code + "\n";
      output += "?:= " + labelStart + ", " + $6.tempRegName + "\n";

      $$ = output;
   }
   ;

E: FOR var ASSIGN NUMBER SEMICOLON bool_expr SEMICOLON var ASSIGN expression BEGINLOOP statement_loop ENDLOOP 
   {
      string iterator_variable = "";
      string output = "";

      if(!$2.isAnArray)
      {
         iterator_variable = $2.code;
      }
      else
      {
         if(!$2.tempRegName.empty())
         {
            iterator_variable = createTempRegister();
            output += $2.code;
            output += ". " + iterator_variable + "\n";
            output += "=[] " + iterator_variable + ", " + $2.name + ", " + $2.index + "\n"; 
         }
      }

      string boolLabel = createLabel();
      string loopBodyLabel = createLabel();
      string exitLabel = createLabel();

      // need to backpatch for continue
      string replacement = ":= " + boolLabel; //go to the start of loop again
      backpatch($12, "continue", replacement);

      output += "= " + iterator_variable + ", " + to_string($4) + "\n";

      output += ": " + boolLabel + "\n";
      output += $6.code + "\n";
      output += "?:= " + loopBodyLabel + ", " + $6.tempRegName + "\n"; //if bool true, go to this label
      output += ":= " + exitLabel + "\n"; //false
      output += ": " + loopBodyLabel + "\n";
      output += $12 + "\n";

      if ($10.tempRegName.empty())
      {
         output += "= " + iterator_variable + ", " + $10.code + "\n";
      }
      else
      {
         output += $10.code + "\n"; // modifiy the temp iterator variable
         output += "= " + iterator_variable + ", " + $10.tempRegName + "\n";
      }
      
      output += ":= " + boolLabel + "\n"; //check bool condition to see if we keep going
      output += ": " + exitLabel + "\n";

      $$ = output;
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

F: READ var_loop
   {
      for (int i = 0; i < $2.size(); ++i) //var_loop is vector
      {
         if ($2[i]->isAnArray)
         {
            if ($2[i]->name.empty())
            {
               $$ += ".[]< " + $2[i]->code + ", " + $2[i]->index + "\n";
            }
            else
            {
               $$ = $2[i]->code;
               $$ += ".[]< " + $2[i]->name + ", " + $2[i]->tempRegName + "\n";
            }
            
         }
         else
         {
            $$ += ".< " + $2[i]->code + "\n";
         }
      }

   }
   ;

G: WRITE var_loop 
   {
      for (int i = 0; i < $2.size(); ++i) //var_loop is vector
      {
         if ($2[i]->isAnArray)
         {
            if ($2[i]->name.empty())
            {
               $$ += ".[]> " + $2[i]->code + ", " + $2[i]->index + "\n";
            }
            else
            {
               $$ = $2[i]->code;
               $$ += ".[]> " + $2[i]->name + ", " + $2[i]->tempRegName + "\n";
            }
            
         }
         else
         {
            $$ += ".> " + $2[i]->code + "\n";
         }
      }

   }
   ;

var_loop:  var 
         {
            node *temp = new node();
            temp->code = $1.code;
            temp->tempRegName = $1.tempRegName; 
            temp->isAnArray = $1.isAnArray; 
            temp->index = $1.index;
            temp->name = $1.name;

            $$.push_back(temp);
         }
         | var COMMA var_loop 
         {
            node *temp = new node();
            temp->code = $1.code;
            temp->tempRegName = $1.tempRegName; 
            temp->isAnArray = $1.isAnArray; 
            temp->index = $1.index;
            temp->name = $1.name;

            $$.push_back(temp);
            $$.insert($$.end(), $3.begin(), $3.end());
         }
         | var var_loop {printf("Syntax error at line %d position %d: Missing comma in variable list.\n", currLine, currPos);}
         ;

H: CONTINUE 
   {
      $$ = "continue" + to_string(@1.begin.line); 
   };

I: RETURN expression 
   {
      string output = "";
      string returnTempName = "";

      if ($2.tempRegName.empty()) //returning just a number
      {
         returnTempName = createTempRegister();
         output += ". " + returnTempName + "\n";
         output += "= " + returnTempName + ", " + $2.code + "\n";
      }
      else  //expression that is complex
      {
         returnTempName = $2.tempRegName;
         output += $2.code + "\n";
      }

      output += "ret " + returnTempName + "\n";
      $$ = output;
   }
   ;

bool_expr: relation_and_expr 
         {
            $$.code = $1.code; 
            $$.tempRegName = $1.tempRegName; 
            $$.isAnArray = $1.isAnArray; 
            $$.index = $1.index;
         }
         | bool_expr OR relation_and_expr 
         {
            string orTempName = createTempRegister();

            $$.code = $1.code + "\n" + $3.code + "\n";
            $$.code += ". " + orTempName + "\n";   //probably should make a funciton for this
            $$.code += "|| " + orTempName + ", " + $1.tempRegName + ", " + $3.tempRegName;
         
            $$.tempRegName = orTempName;
         }
   ;

relation_and_expr: relation_expr 
                  {
                     $$.code = $1.code; 
                     $$.tempRegName = $1.tempRegName; 
                     $$.isAnArray = $1.isAnArray; 
                     $$.index = $1.index;
                  }
                  | relation_and_expr AND relation_expr
                  {
                     string andTempName = createTempRegister();

                     $$.code = $1.code + "\n" + $3.code + "\n";
                     $$.code += ". " + andTempName + "\n";   //probably should make a funciton for this
                     $$.code += "&& " + andTempName + ", " + $1.tempRegName + ", " + $3.tempRegName;
                  
                     $$.tempRegName = andTempName;
                  }
   ;

relation_expr: relations 
            {
               $$.code = $1.code; 
               $$.tempRegName = $1.tempRegName; 
               $$.isAnArray = $1.isAnArray; 
               $$.index = $1.index;
            }
            | NOT relations
            {
               string output = "";
               string negationTempName = createTempRegister();

               output += $2.code + "\n";
               output += ". " + negationTempName + "\n";
               output += "! " + negationTempName + ", " + $2.tempRegName + "\n";

               $$.code = output;
               $$.tempRegName = negationTempName;
               $$.isAnArray = $2.isAnArray; 
               $$.index = $2.index;
            }
            ;
relations:  expression comp expression 
            {
               // same as expression ADD multiplicative_expr logic
               string relationTempName = createTempRegister();
               string firstExpression;
               string output = "";

               if ($1.tempRegName.empty()) //so its a number
               {
                  firstExpression = $1.code;
               }
               else  // the expression is complex
               {
                  output += $1.code + "\n";
                  firstExpression = $1.tempRegName;
               }

               if ($3.tempRegName.empty()) //single variable, not conplex
               {
                  output += ". " + relationTempName + "\n";
                  output += $2 + " " + relationTempName + ", " + firstExpression + ", " + $3.code;
               }
               else
               {
                  output += $3.code;
                  output += ". " + relationTempName + "\n";
                  output += $2 + " " + relationTempName + ", " + firstExpression + ", " + $3.tempRegName;
               }

               $$.code = output;
               $$.tempRegName = relationTempName;
               $$.isAnArray = false;
               $$.index = "";
            }
         |  TRUE 
         {
            string trueTempName = createTempRegister();

            $$.code = ". " + trueTempName + "\n";
            $$.code += "= " + trueTempName + ", 1";
            $$.tempRegName = trueTempName;
         }
         |  FALSE
         {
            string trueTempName = createTempRegister();
            
            $$.code = ". " + trueTempName + "\n";
            $$.code += "= " + trueTempName + ", 0";
            $$.tempRegName = trueTempName;
         }
         |  L_PAREN bool_expr R_PAREN 
         {
            $$.code = $2.code; 
            $$.tempRegName = $2.tempRegName; 
            $$.isAnArray = $2.isAnArray; 
            $$.index = $2.index;
         }
         ;

comp: EQ {$$ = "==";}
   |  NEQ {$$ = "!=";}
   |  LT {$$ = "<";}
   |  GT {$$ = ">";}
   |  LTE {$$ = "<=";}
   |  GTE {$$ = ">=";}
   ;

expression: multiplicative_expr 
         {$$.code = $1.code; $$.tempRegName = $1.tempRegName; $$.isAnArray = $1.isAnArray; $$.index = $1.index;}
         | expression ADD multiplicative_expr
         {
            string tempName = createTempRegister();
            string secondOperator;
            string output = "";

            if ($1.tempRegName.empty()) //so its a number
            {
               secondOperator = $1.code;
            }
            else  //more than two numbers
            {
               output += $1.code;
               secondOperator = $1.tempRegName;
            }

            if ($3.tempRegName.empty()) //also a number
            {
               output += ". " + tempName + "\n";
               output += "+ " + tempName + ", " + secondOperator + ", " + $3.code + "\n";
            }
            else // TODO: NOT WORKING I THINK
            {
               output += $3.code;
               output += ". " + tempName + "\n";
               output += "+ " + tempName + ", " + secondOperator + ", " + $3.tempRegName + "\n";
            }

            $$.code = output;
            $$.tempRegName = tempName;
            $$.isAnArray = false;
            $$.index = "";
         }
         | expression SUB multiplicative_expr 
         {
            string tempName = createTempRegister();
            string secondOperator;
            string output = "";

            if ($1.tempRegName.empty()) //so its a number
            {
               secondOperator = $1.code;
            }
            else  //more than two numbers
            {
               output += $1.code;
               secondOperator = $1.tempRegName;
            }

            if ($3.tempRegName.empty()) //also a number
            {
               output += ". " + tempName + "\n";
               output += "- " + tempName + ", " + secondOperator + ", " + $3.code + "\n";
            }
            else // TODO: NOT WORKING I THINK
            {
               output += $3.code;
               output += ". " + tempName + "\n";
               output += "- " + tempName + ", " + secondOperator + ", " + $3.tempRegName + "\n";
            }

            $$.code = output;
            $$.tempRegName = tempName;
            $$.isAnArray = false;
            $$.index = "";
         }
         ;

multiplicative_expr: term {$$.code = $1.code; $$.tempRegName = $1.tempRegName; $$.isAnArray = $1.isAnArray; $$.index = $1.index;}
                  | multiplicative_expr MULT term 
                  {
                     string multTempName = createTempRegister();
                     string secondOperator;
                     string output = "";

                     if ($1.tempRegName.empty()) //so its a number
                     {
                        secondOperator = $1.code;
                     }
                     else  //more than two numbers
                     {
                        output += $1.code;
                        secondOperator = $1.tempRegName;
                     }

                     if ($3.tempRegName.empty()) //also a number
                     {
                        output += ". " + multTempName + "\n";
                        output += "* " + multTempName + ", " + secondOperator + ", " + $3.code + "\n";
                     }
                     else // TODO: NOT WORKING I THINK
                     {
                        output += $3.code;
                        output += ". " + multTempName + "\n";
                        output += "* " + multTempName + ", " + secondOperator + ", " + $3.tempRegName + "\n";
                     }

                     $$.code = output;
                     $$.tempRegName = multTempName;
                     $$.isAnArray = false;
                     $$.index = "";

                  }
                  | multiplicative_expr DIV term 
                  {
                     string divTempName = createTempRegister();
                     string secondOperator;
                     string output = "";

                     if ($1.tempRegName.empty()) //so its a number
                     {
                        secondOperator = $1.code;
                     }
                     else  //more than two numbers
                     {
                        output += $1.code;
                        secondOperator = $1.tempRegName;
                     }

                     if ($3.tempRegName.empty()) //also a number
                     {
                        output += ". " + divTempName + "\n";
                        output += "/ " + divTempName + ", " + secondOperator + ", " + $3.code + "\n";
                     }
                     else // TODO: NOT WORKING I THINK
                     {
                        output += $3.code;
                        output += ". " + divTempName + "\n";
                        output += "/ " + divTempName + ", " + secondOperator + ", " + $3.tempRegName + "\n";
                     }

                     $$.code = output;
                     $$.tempRegName = divTempName;
                     $$.isAnArray = false;
                     $$.index = "";

                  }
                  | multiplicative_expr MOD term
                  {
                     string modTempName = createTempRegister();
                     string secondOperator;
                     string output = "";

                     if ($1.tempRegName.empty()) //so its a number
                     {
                        secondOperator = $1.code;
                     }
                     else  //more than two numbers
                     {
                        output += $1.code;
                        secondOperator = $1.tempRegName;
                     }

                     if ($3.tempRegName.empty()) //also a number
                     {
                        output += ". " + modTempName + "\n";
                        output += "% " + modTempName + ", " + secondOperator + ", " + $3.code + "\n";
                     }
                     else // TODO: NOT WORKING I THINK
                     {
                        output += $3.code;
                        output += ". " + modTempName + "\n";
                        output += "% " + modTempName + ", " + secondOperator + ", " + $3.tempRegName + "\n";
                     }

                     $$.code = output;
                     $$.tempRegName = modTempName;
                     $$.isAnArray = false;
                     $$.index = "";
                  }
                  ;

term: term_top 
      {
         if ($1.type.compare("variable") == 0)
         {
            string newTempRegName = createTempRegister();

            if ($1.isAnArray)
            {
               $$.code = $1.code;
               $$.code += ". " + newTempRegName + "\n";
               $$.code += "=[] " + newTempRegName + ", " + $1.name + ", " + $1.index + "\n";
            }
            else
            {
               $$.code = ". " + newTempRegName + "\n";
               $$.code += "= " + newTempRegName + ", " + $1.code + "\n";
            }

            $$.tempRegName = newTempRegName;
            $$.index = $1.index;
            $$.isAnArray = $1.isAnArray;
         }
         else if ($1.type.compare("number") == 0)
         {
            string numberTempName = createTempRegister();
            $$.code = ". " + numberTempName + "\n";
            $$.code += "= " + numberTempName + ", " + $1.code + "\n";
            $$.tempRegName = numberTempName;
            $$.isAnArray = $1.isAnArray;
            $$.index = $1.index;
         }
         else //TODO
         {
            $$.code = $1.code;
            $$.tempRegName = $1.tempRegName;
            $$.isAnArray = $1.isAnArray;
            $$.index = $1.index;
         }
      }
   |  SUB term_top %prec UMINUS 
   {
      string subTemp = createTempRegister();
      if ($2.type.compare("variable") == 0)
         {
            string newTempRegName = createTempRegister();

            if ($2.isAnArray)
            {
               $$.code = $2.code;
               $$.code += ". " + newTempRegName + "\n";
               $$.code += "=[] " + newTempRegName + ", " + $2.name + ", " + $2.index + "\n";
            }
            else
            {
               $$.code = ". " + newTempRegName + "\n";
               $$.code += "= " + newTempRegName + ", " + $2.code + "\n";
            }

            $$.code += ". " + subTemp + "\n";
            $$.code += "- " + subTemp + ", 0, " + newTempRegName + "\n";

            $$.tempRegName = subTemp;
            $$.index = $2.index;
            $$.isAnArray = $2.isAnArray;
         }
         else if ($2.type.compare("number") == 0)
         {
            string numberTempName = createTempRegister();
            $$.code = ". " + numberTempName + "\n";
            $$.code += "= " + numberTempName + ", " + $2.code + "\n";

            $$.code += ". " + subTemp + "\n";
            $$.code += "- " + subTemp + ", 0, " + numberTempName + "\n";

            $$.tempRegName = subTemp;
            $$.isAnArray = $2.isAnArray;
            $$.index = $2.index;
         }
         else //TODO
         {
            $$.code = $2.code + "\n";
            $$.code += ". " + subTemp + "\n";
            $$.code += "- " + subTemp + ", 0, " + $2.tempRegName + "\n";

            $$.tempRegName = subTemp;
            $$.isAnArray = $2.isAnArray;
            $$.index = $2.index;
         }
   }
   |  ident L_PAREN term_exp R_PAREN
   {
      sym_table.push_back({$1, @1, make_pair(-1,-1), CALL});

      string output = "";
      string functionTempName = createTempRegister();

      for (int i = 0; i < $3.size(); ++i)
      {
         output += $3[i]->code + "\n";
         output += "param " + $3[i]->tempRegName + "\n";
      }

      output += ". " + functionTempName + "\n";
      output += "call " + $1 + ", " + functionTempName + "\n";

      $$.code = output;
      $$.tempRegName = functionTempName;

   }
   |  ident L_PAREN R_PAREN
   {
      sym_table.push_back({$1, @1, make_pair(-1,-1), CALL});

      string output = "";
      string functionTempName = createTempRegister();

      output += ". " + functionTempName + "\n";
      output += "call " + $1 + ", " + functionTempName + "\n";

      $$.code = output;
      $$.tempRegName = functionTempName;
   }
   ;

term_exp:   expression 
         {
            node *temp = new node();
            temp->code = $1.code;
            temp->tempRegName = $1.tempRegName; 
            temp->isAnArray = $1.isAnArray; 
            temp->index = $1.index;

            $$.push_back(temp);
         }
         |  expression COMMA term_exp 
         {
            node *temp = new node();
            temp->code = $1.code;
            temp->tempRegName = $1.tempRegName; 
            temp->isAnArray = $1.isAnArray; 
            temp->index = $1.index;

            $$.push_back(temp);
            $$.insert($$.end(), $3.begin(), $3.end());
         }
         |  expression error term_exp {yyerrok; yyerror("Syntax error, missing comma inbetween expressions.");}
         ;

term_top: var 
         {
            $$.code = $1.code; 
            $$.tempRegName = $1.tempRegName; 
            $$.isAnArray = $1.isAnArray; 
            $$.index = $1.index;
            $$.type = "variable";
            $$.name = $1.name;
         }
      |  NUMBER 
      {
         $$.code = to_string($1); 
         $$.tempRegName = ""; 
         $$.isAnArray = false; 
         $$.index = "";
         $$.type = "number";
      }
      |  L_PAREN expression R_PAREN 
      {
         $$.code = $2.code;
         $$.tempRegName = $2.tempRegName;
         $$.isAnArray = $2.isAnArray;
         $$.index = $2.isAnArray;
         $$.type = "";
      }
      ;

var:  ident 
   {
      // Error 1
      type tempType = ARRAY;
      int temp = inSymbolTable($1);
      if (temp >= 0) //means that it was found
      {
         // Error 6
         if (sym_table.at(temp).type_var == tempType)
         {
            errorHasOccured = true;
            yy::parser::error(@1, "used array variable \"" + $1 + "\" is missing specified index.");
         }
         else
         {
            $$.code = $1; 
            $$.index = ""; 
            $$.isAnArray = false; 
            $$.tempRegName = "";
            $$.name = "";
         }
      }
      else
      {
         errorHasOccured = true;
         yy::parser::error(@1, "used variable \"" + $1 + "\" was not previously declared.");
      }
   }
   |  ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET 
      {
         // Error 1
         int temp = inSymbolTable($1);
         type tempType = INTEGER;
         if (temp >= 0) //means that it was found
         {
            // Error 7
            if (sym_table.at(temp).type_var == tempType)
            {
               errorHasOccured = true;
               yy::parser::error(@1, "used integer variable \"" + $1 + "\" as an array variable.");
            }
         }
         else
         {
            errorHasOccured = true;
            yy::parser::error(@1, "used variable \"" + $1 + "\" was not previously declared.");
         }

         string output = "";
         string index = "";
         if ($3.tempRegName.empty())
         {
            output += $1;
            index = $3.code;
            $$.name = "";
            $$.tempRegName = "";
         }
         else
         {
            output += $3.code + "\n";
            index = $3.tempRegName;
            $$.name = $1;
            $$.tempRegName = index;
         }
         $$.code = output;
         $$.index = index;
         $$.isAnArray = true;
      }
   |  ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET
      {
         // TODO 2-D ARRAY
         printf("var -> ident L_SQUARE_BRACKET expression R_SQUARE_BRACKET L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n");
      }
   ;

ident:   IDENT {$$ = $1;}
      ;
%%

string createTempRegister()
{
   static int regNum = 0;
   return "_temp_" + to_string(regNum++);
}

string createLabel()
{
   static int label_num = 0;
   return "_label_" + to_string(label_num++);
}

void replaceNewlines(string &str, const string &target, const string &new_string)
{
   size_t start_pos = 0;
   while((start_pos = str.find(target, start_pos)) != string::npos)
   {
      str.replace(start_pos, target.length(), new_string);
      start_pos += new_string.length();
   }
}

void backpatch(string &str, const string &target, const string &new_string)
{
   regex reg(target + ".+");
   str = regex_replace(str, reg, new_string);
}

int checkContinueLoop(string &str)
{
   istringstream f(str);
   string line;
   int count = 0;

   while(getline(f, line))
   {  
      if(line.find("continue") != string::npos)
      {
         count++;
      }
   }
   return count;
}

int inSymbolTable(string &s)
{
   for(int i = 0; i < sym_table.size(); ++i)
   {
      if (sym_table.at(i).name.compare(s) == 0)
      {
         return i;
      }
   }
   return -1;
}

bool hasMain()
{
   for(auto name : functionNames)
   {
      if(name.compare("main") == 0)
      {
         return true;
      }
   }
   return false;
}

void extractFunctionCalls()
{
   for(auto item : sym_table)
   {
      if(item.type_var == CALL)
      {
         functionCallsOnly.push_back(item);
      }
   }
}

bool errorTwo()
{
   for(int i = 0; i < functionNames.size(); ++i)
   {
      functionCallsOnly.erase(remove_if(functionCallsOnly.begin(), functionCallsOnly.end(),
      [&](tests const& name){return (name.name.compare(functionNames.at(i)) == 0);}), functionCallsOnly.end());
   }

   return (!functionCallsOnly.empty());
}

int main(int argc, char *argv[])
{
	yy::parser p;
	return p.parse();
}

void yy::parser::error(const yy::location& l, const std::string& m)
{
	std::cerr << "Error line " << l << ": " << m << std::endl;
}

void yyerror(string s)
{
   std::cerr << s << std::endl;  //use this just to write to terminal
}
void yyerror(const char *msg)
{
    cout << msg << endl;
}