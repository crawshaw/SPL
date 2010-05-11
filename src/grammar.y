%{
#include "ast.h"
#include <iostream>
using namespace SPL;

void yyerror(const char *error);
int yylex();

static std::vector<std::string> args;
%}

%locations
%token IDENT NUMBER

%union {
  AST::Expr *exp;
  int value;
  std::string *ident;
}

%type <exp>  exp
%type <value> NUMBER
%type <ident> IDENT

%token DEF "def"
%token '('
%token ')'
%token ','
%left '+'
%left '*'
%%

exp : exp '+' exp { $$ = new AST::Add(*$1, *$3); }
    | exp '-' exp { $$ = new AST::Subtract(*$1, *$3); }
    | exp '*' exp { $$ = new AST::Multiply(*$1, *$3); }
    | exp ';' exp { $$ = new AST::Seq(*$1, *$3); }
    | '{' exp '}' { $$ = $2; }
    | IDENT       { $$ = new AST::Variable(*$1); }
    | NUMBER      { $$ = new AST::Number($1); }
    | DEF IDENT '(' args ')' '=' exp {
      std::vector<std::string> arguments = args;
      args.clear();
      $$ = new AST::Function(*$2, arguments, *$7);
    }

args :
     | IDENT { args.push_back(*$1); }
     | args ',' IDENT { args.push_back(*$3); }

%%

int main()
{
  return yyparse();
}

void yyerror(const char *error)
{
  fprintf(stderr, "%d.%d-%d.%d: error: %s\n",
    yylloc.first_line, yylloc.first_column,
    yylloc.last_line, yylloc.last_column,
    error
  );
}

int yylex() {
  char LastChar = std::cin.get();

  while (isspace(LastChar))
    LastChar = std::cin.get();

  if (isalpha(LastChar)) {
    std::string StrVal = "";
    StrVal += LastChar;
    while (isalnum((std::cin.peek())))
      StrVal += std::cin.get();

    if (StrVal == "def") return DEF;
    /*
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
    num += LastChar;
    while (isdigit(std::cin.peek()))
      num += std::cin.get();

    int NumVal;
    SPL::Util::from_string<int>(NumVal, num, std::dec);
    yylval.value = NumVal;
    return NUMBER;
  }

  if (LastChar == EOF)
    return -1;

  return LastChar;
}
