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

%token DEF
%token IO
%token IMP
%token VAR
%token VAL
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
      $$ = new AST::Function(*$2, arguments, *$7, AST::Pure);
    }
    | IO IDENT '(' args ')' '=' exp {
      std::vector<std::string> arguments = args;
      args.clear();
      $$ = new AST::Function(*$2, arguments, *$7, AST::FunIO);
    }
    | IMP IDENT '(' args ')' '=' exp {
      std::vector<std::string> arguments = args;
      args.clear();
      $$ = new AST::Function(*$2, arguments, *$7, AST::Impure);
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
    if (StrVal == "io")  return IO;
    if (StrVal == "imp") return IMP;
    if (StrVal == "var") return VAR;
    if (StrVal == "val") return VAL;

    yylval.ident = new std::string(StrVal);
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
