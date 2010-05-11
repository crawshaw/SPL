%{
#include "ast.h"
#include <iostream>
using namespace SPL;

void yyerror(const char *error);
int yylex();
%}

%token IDENT NUMBER

%union {
  AST::Expr *exp;
  int value;
  std::string *ident;
}

%type <exp>  exp
%type <value> NUMBER
%type <ident> IDENT

%left '+'
%left '*'
%%

exp : exp '+' exp { $$ = new AST::Add(*$1, *$3); }
    | exp '-' exp { $$ = new AST::Subtract(*$1, *$3); }
    | exp '*' exp { $$ = new AST::Multiply(*$1, *$3); }
    | IDENT       { $$ = new AST::Variable(*$1); }
    | NUMBER      { $$ = new AST::Number($1); }
    | "foo" { $$ = new AST::Number(4); }
    | "def" IDENT '(' IDENT ',' IDENT ')' {
      std::vector<std::string> args;
      std::vector<AST::Expr*> body;
      $$ = new AST::Function(*$2, args, body);
    }

%%

int main()
{
  return yyparse();
}

void yyerror(const char *error)
{
  std::cout << error << std::endl;
}

int yylex() {
  char LastChar = std::cin.get();

  while (isspace(LastChar))
    LastChar = std::cin.get();

  if (isalpha(LastChar)) {
    std::string StrVal = "";
    StrVal += LastChar;
    while (isalnum((LastChar = std::cin.get())))
      StrVal += LastChar;

    /*
    if (StrVal == "def") return tok_def;
    if (StrVal == "io")  return tok_io;
    if (StrVal == "imp") return tok_imp;
    if (StrVal == "var") return tok_var;
    if (StrVal == "val") return tok_val;
    */

    std::string *ident = new std::string(StrVal);
    yylval.ident = ident;
    return IDENT;
  }

  if (isdigit(LastChar)) {
    std::string num;
    do {
      num += LastChar;
      LastChar = std::cin.get();
    } while (isdigit(LastChar));
    int NumVal;
    SPL::Util::from_string<int>(NumVal, num, std::dec);
    yylval.value = NumVal;
    return NUMBER;
  }

  if (LastChar == EOF)
    return -1;

  int ThisChar = LastChar;
  LastChar = std::cin.get();
  return ThisChar;
}
