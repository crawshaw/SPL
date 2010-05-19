%{
#include "ast.h"
#include <iostream>
#include <fstream>
using namespace SPL;
using namespace std;

void yyerror(const char *error);
int yylex();

static std::vector<AST::Func*>      toplevel;
static std::vector<AST::SType*>     types;
static std::vector<AST::Class*>     classes;
static std::vector<AST::Instance*>  instances;
%}

%locations
%token IDENT TIDENT NUMBER

%union {
  AST::Expr *exp;
  AST::Func *fun;
  int value;
  string *ident;
  vector<pair<string,string*>*> *args;
  vector<AST::Expr*> *callargs;
  AST::SType *type;
  AST::Class *cls;
  AST::Instance *instance;
}

%type <exp>       exp
%type <value>     NUMBER
%type <ident>     IDENT
%type <ident>     TIDENT
%type <args>      args
%type <callargs>  callargs
%type <fun>       fun
%type <type>      sstruct
%type <type>      sunion
%type <cls>       class
%type <instance>  instance

// TODO: add TK_
%token DEF
%token IO
%token IMP
%token VAR
%token TK_VAL
%token IF
%token THEN
%token ELSE
%token STRUCT
%token UNION
%token CLASS
%token INSTANCE
%token EQ "=="
%token '='
%token '('
%token ')'
%token ','
%token ':'
%left '+'
%left '*'
%%

top :
    | fun top       { toplevel.push_back($1); }
    | sstruct top   { types.push_back($1); }
    | sunion top    { types.push_back($1); }
    | class top     { classes.push_back($1); }
    | instance top  { instances.push_back($1); }

exp : exp '+' exp { $$ = new AST::Add(*$1, *$3); }
    | exp '-' exp { $$ = new AST::Subtract(*$1, *$3); }
    | exp '*' exp { $$ = new AST::Multiply(*$1, *$3); }
    | exp ';' exp { $$ = new AST::Seq(*$1, *$3); }
    | exp '.' exp { $$ = new AST::Member(*$1, *$3); }
    | exp EQ  exp { $$ = new AST::Eq(*$1, *$3); }
    | '{' exp '}' { $$ = $2; }
    | IDENT       { $$ = new AST::Variable(*$1); }
    | NUMBER      { $$ = new AST::Number($1); }
    | IF exp THEN exp ELSE exp  { $$ = new AST::If(*$2, *$4, *$6); }
    | TK_VAL IDENT '=' exp ';' exp { $$ = new AST::Binding(*$2, *$4, *$6); }
    | IDENT '(' callargs ')' { $$ = new AST::Call(*$1, *$3); }
    | fun ';' exp {
      $1->setContext(*$3);
      AST::Expr* expr = $1;
      $$ = expr;
    }

fun : DEF IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, NULL, *$8, NULL, AST::Pure);
    }
    | DEF IDENT '(' args ')' ':' IDENT '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, $7, *$10, NULL, AST::Pure);
    }
    | IO IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, NULL, *$8, NULL, AST::FunIO);
    }
    | IO IDENT '(' args ')' ':' IDENT '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, $7, *$10, NULL, AST::FunIO);
    }
    | IMP IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, NULL, *$8, NULL, AST::Impure);
    }
    | IMP IDENT '(' args ')' ':' IDENT '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, $7, *$10, NULL, AST::Impure);
    }

args  : {$$ = new vector<pair<string,string*>*>(); }
      | IDENT ':' TIDENT {
        $$ = new vector<pair<string,string*>*>();
        $$->push_back(new pair<string,string*>(*$1, $3));
      }
      | IDENT {
        $$ = new vector<pair<string,string*>*>();
        $$->push_back(new pair<string,string*>(*$1, NULL));
      }
      | args ',' IDENT ':' TIDENT {
        $1->push_back(new pair<string,string*>(*$3, $5));
        $$ = $1;
      }
      | args ',' IDENT {
        $1->push_back(new pair<string,string*>(*$3, NULL));
        $$ = $1;
      }

callargs
      : { $$ = new std::vector<AST::Expr*>(); }
      | exp { $$ = new std::vector<AST::Expr*>(); $$->push_back($1); }
      | callargs ',' exp { $1->push_back($3); $$ = $1; }

sstruct : STRUCT TIDENT '=' '{' args '}' {
  $$ = new AST::SStructType(*$2, *$5);
}
sunion  : UNION  TIDENT '=' '{' args '}' { $$ = NULL; /* TODO */ }

class : CLASS IDENT { $$ = NULL; /* TODO */ }

instance : INSTANCE IDENT { $$ = NULL; /* TODO */ }

%%

static std::istream *codein;

void usage() {
  std::cerr << "usage: spl <filename>" << std::endl;
  exit(1);
}

int main(int argc, const char* argv[])
{
  if (argc != 2)
    usage();

  std::string fileName(argv[1]);
  std::ifstream infile(argv[1]);
  if (!infile.is_open())
    usage();
  codein = &infile;

  int ret = yyparse();
  if (ret)
    return ret;

  std::cout << "Parsed, " << toplevel.size() << " functions." << std::endl;
  AST::File file(fileName, toplevel, types);

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
    case '.':
    case '+': // TODO: remove these special cases, treat as identifiers
    case '-':
    case '*':
      return true;
    default:
      return false;
  }
}

int yylex() {
  int LastChar = codein->get();

  while (isspace(LastChar))
    LastChar = codein->get();

  if (isdigit(LastChar)) {
    std::string num;
    num += LastChar;
    while (isdigit(codein->peek()))
      num += codein->get();

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
  while (!isspace(codein->peek()) && !isaspecial(codein->peek()))
    StrVal += codein->get();

  if (StrVal == "def") return DEF;
  if (StrVal == "io")  return IO;
  if (StrVal == "imp") return IMP;
  if (StrVal == "var") return VAR;
  if (StrVal == "val") return TK_VAL;
  if (StrVal == "if")   return IF;
  if (StrVal == "then") return THEN;
  if (StrVal == "else") return ELSE;
  if (StrVal == "struct") return STRUCT;
  if (StrVal == "union") return UNION;
  if (StrVal == "class") return CLASS;
  if (StrVal == "instance") return INSTANCE;
  if (StrVal == "==") return EQ;
  if (StrVal == "=") return '=';

  yylval.ident = new std::string(StrVal);
  if (StrVal[0] >= 'A' && StrVal[0] <= 'Z')
    return TIDENT;
  else
    return IDENT;
}
