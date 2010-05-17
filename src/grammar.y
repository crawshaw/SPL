%{
#include "ast.h"
#include <iostream>
using namespace SPL;

void yyerror(const char *error);
int yylex();

static std::vector<AST::Func*> toplevel;
%}

%locations
%token IDENT NUMBER

%union {
  AST::Expr *exp;
  AST::Func *fun;
  int value;
  std::string *ident;
  std::vector<std::string> *args;
  std::vector<AST::Expr*> *callargs;
}

%type <exp>       exp
%type <value>     NUMBER
%type <ident>     IDENT
%type <args>      args
%type <callargs>  callargs
%type <fun>       fun

// TODO: add TK_
%token DEF
%token IO
%token IMP
%token VAR
%token TK_VAL
%token IF
%token THEN
%token ELSE
%token EQ "=="
%token '='
%token '('
%token ')'
%token ','
%left '+'
%left '*'
%%

top :
    | fun top { toplevel.push_back($1); }

exp : exp '+' exp { $$ = new AST::Add(*$1, *$3); }
    | exp '-' exp { $$ = new AST::Subtract(*$1, *$3); }
    | exp '*' exp { $$ = new AST::Multiply(*$1, *$3); }
    | exp ';' exp { $$ = new AST::Seq(*$1, *$3); }
    | exp EQ  exp { $$ = new AST::Eq(*$1, *$3); }
    | '{' exp '}' { $$ = $2; }
    | IDENT       { $$ = new AST::Variable(*$1); }
    | NUMBER      { $$ = new AST::Number($1); }
    | IF exp THEN exp ELSE exp  { $$ = new AST::If(*$2, *$4, *$6); }
    | TK_VAL IDENT '=' exp ';' exp { $$ = new AST::Bind(*$2, *$4, *$6); }
    | IDENT '(' callargs ')' { $$ = new AST::Call(*$1, *$3); }
    | fun ';' exp {
      $1->setContext(*$3);
      AST::Expr* expr = $1;
      $$ = expr;
    }

fun : DEF IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, *$8, NULL, AST::Pure);
    }
    | IO IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, *$8, NULL, AST::FunIO);
    }
    | IMP IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, *$8, NULL, AST::Impure);
    }

args  : { $$ = new std::vector<std::string>(); }
      | IDENT { $$ = new std::vector<std::string>(); $$->push_back(*$1); }
      | args ',' IDENT { $1->push_back(*$3); $$ = $1; }

callargs
      : { $$ = new std::vector<AST::Expr*>(); }
      | exp { $$ = new std::vector<AST::Expr*>(); $$->push_back($1); }
      | callargs ',' exp { $1->push_back($3); $$ = $1; }

%%

int main()
{
  int ret = yyparse();
  if (ret)
    return ret;

  std::string fileName("<stdin>");
  std::cout << "Parsed, " << toplevel.size() << " functions." << std::endl;
  AST::File file(fileName, toplevel);

  file.run();

  return 0;
}

void yyerror(const char *error)
{
  fprintf(stderr, "%d.%d-%d.%d: error: %s\n",
    yylloc.first_line, yylloc.first_column,
    yylloc.last_line, yylloc.last_column,
    error
  );
}

bool isaspecial(int ch) {
  switch (ch) {
    case ')':
    case '(':
    case '{':
    case '}':
    case ',':
    case ':':
    case ';':
    case '"':
    case '`':
    case '[':
    case ']':
    case '+': // TODO: remove these special cases, treat as identifiers
    case '-':
    case '*':
      return true;
    default:
      return false;
  }
}

int yylex() {
  int LastChar = std::cin.get();

  while (isspace(LastChar))
    LastChar = std::cin.get();

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

  if (isaspecial(LastChar))
    return LastChar;

  // Collect until whitespace.
  std::string StrVal = "";
  StrVal += LastChar;
  while (!isspace(std::cin.peek()) && !isaspecial(std::cin.peek()))
    StrVal += std::cin.get();

  if (StrVal == "def") return DEF;
  if (StrVal == "io")  return IO;
  if (StrVal == "imp") return IMP;
  if (StrVal == "var") return VAR;
  if (StrVal == "val") return TK_VAL;
  if (StrVal == "if")   return IF;
  if (StrVal == "then") return THEN;
  if (StrVal == "else") return ELSE;
  if (StrVal == "==") return EQ;
  if (StrVal == "=") return '=';

  yylval.ident = new std::string(StrVal);
  return IDENT;
}
