%{
#include "ast.h"
#include <iostream>
#include <fstream>
using namespace SPL;
using namespace std;

void yyerror(const char *error);
int yylex();

std::vector<AST::Func*>      toplevel;
std::vector<AST::Extern*>    externs;
std::vector<AST::SType*>     types;
std::vector<AST::Class*>     classes;
std::vector<AST::Instance*>  instances;
%}

%locations
%token IDENT TIDENT NUMBER STRING ARRAY

%union {
  AST::Expr *exp;
  AST::Func *fun;
  AST::Extern *ext;
  int value;
  string *ident;
  vector<pair<string,string*>*> *args;
  vector<AST::Expr*> *callargs;
  vector<string> *templates;
  AST::SType *type;
  AST::Class *cls;
  AST::Instance *instance;
  AST::Purity purity;
}

%type <exp>       exp
%type <value>     NUMBER
%type <ident>     STRING
%type <ident>     IDENT
%type <ident>     TIDENT
%type <args>      args
%type <callargs>  callargs
%type <callargs>  exps
%type <fun>       fun
%type <exp>       EXTERN
%type <ext>       extern
%type <type>      sstruct
%type <type>      sunion
%type <cls>       class
%type <instance>  instance
%type <templates> templates
%type <templates> templateSet
%type <templates> targs
%type <purity>    funDef

// TODO: add TK_
%token DEF
%token IO
%token IMP
%token EXTERN
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
%token PP "++"
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
    | extern top    { externs.push_back($1); }
    | sstruct top   { types.push_back($1); }
    | sunion top    { types.push_back($1); }
    | class top     { classes.push_back($1); }
    | instance top  { instances.push_back($1); }

exp : exp '+' exp { $$ = new AST::Add(*$1, *$3); }
    | exp '-' exp { $$ = new AST::Subtract(*$1, *$3); }
    | exp '*' exp { $$ = new AST::Multiply(*$1, *$3); }
    | exp '=' exp { $$ = new AST::Assign(*$1, *$3); }
    | exp '.' IDENT { $$ = new AST::Member(*$1, *$3); }
    | exp EQ  exp { $$ = new AST::Eq(*$1, *$3); }
    | exp PP  exp { $$ = new AST::JoinString(*$1, *$3); }
    | '{' exps '}' {
      // Unroll the block into a classic ML expression.
      vector<AST::Expr*> exprs = *$2;
      if (exprs.size() == 0) {
        std::cerr << "Empty block" << std::endl;
        exit(1);
      } else if (exprs.size() == 1) {
        $$ = exprs[0];
      } else {
        AST::Expr *last = exprs[exprs.size() - 1];
        for (int i=exprs.size() - 2; i >= 0; --i) {
          if (AST::Func *fn = dynamic_cast<AST::Func*>(exprs[i])) {
            fn->setContext(*last);
            last = fn;
          } else if (AST::Binding *bn = dynamic_cast<AST::Binding*>(exprs[i])) {
            bn->setBody(*last);
            last = bn;
          } else {
            last = new AST::Seq(*exprs[i], *last);
          }
        }
        $$ = last;
      }
    }
    | IDENT       { $$ = new AST::Variable(*$1); }
    | NUMBER      { $$ = new AST::Number($1); }
    | STRING      { $$ = new AST::StringLiteral(*$1); }
    | exp '[' exp ']' { $$ = new AST::ArrayAccess(*$1, *$3); }
    | IF exp THEN exp ELSE exp  { $$ = new AST::If(*$2, *$4, *$6); }
    | TK_VAL IDENT '=' exp { $$ = new AST::Binding(*$2, *$4); }
    | IDENT '(' callargs ')' { $$ = new AST::Call(*$1, *$3); }
    | ARRAY '[' TIDENT ']' '(' exp ')' { $$ = new AST::Array(*$3, *$6); }
    | TIDENT '(' callargs ')' { $$ = new AST::Constructor(*$1, *$3); }
    | fun { $$ = $1; }

exps  : { $$ = new vector<AST::Expr*>(); }
      | exps ';' exp { $$ = $1; $$->push_back($3); }
      | exp { $$ = new vector<AST::Expr*>(); $$->push_back($1); }

fun : funDef IDENT templateSet '(' args ')' ':' TIDENT '=' exp {
      $$ = new AST::Func(*$2, *$3, *$5, $8, *$10, NULL, $1);
    }
    /* TODO: when we have more than local type inference working, invoke.
    | funDef IDENT '(' args ')' '=' '{' exp '}' {
      $$ = new AST::Func(*$2, *$4, NULL, *$8, NULL, $1);
    }
    */

extern : EXTERN IDENT '(' targs ')' ':' TIDENT { $$ = new AST::Extern(*$2,*$4,*$7); }

funDef : DEF  { $$ = AST::Pure }
       | IO   { $$ = AST::FunIO }
       | IMP  { $$ = AST::Impure }

templateSet : { $$ = new vector<string>(); }
          | '<' templates '>' { $$ = $2; }

templates : { $$ = new vector<string>(); }
          | templates ',' TIDENT { $1->push_back(*$3); $$ = $1; }
          | TIDENT { $$ = new vector<string>(); $$->push_back(*$1); }

targs : { $$ = new vector<string>(); }
      | TIDENT {
        $$ = new vector<string>();
        $$->push_back(*$1);
      }
      | targs ',' TIDENT {
        $1->push_back(*$3);
        $$ = $1;
      }

args  : {$$ = new vector<pair<string,string*>*>(); }
      | IDENT ':' TIDENT {
        $$ = new vector<pair<string,string*>*>();
        $$->push_back(new pair<string,string*>(*$1, $3));
      }
      /* TODO
      | IDENT {
        $$ = new vector<pair<string,string*>*>();
        $$->push_back(new pair<string,string*>(*$1, NULL));
      }
      */
      | args ',' IDENT ':' TIDENT {
        $1->push_back(new pair<string,string*>(*$3, $5));
        $$ = $1;
      }
      /* TODO
      | args ',' IDENT {
        $1->push_back(new pair<string,string*>(*$3, NULL));
        $$ = $1;
      }
      */

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

std::istream *codein;
int line;
int col;

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
    //case '+': // TODO: remove these special cases, treat as identifiers
    case '-':
    case '*':
    case '<':
    case '>':
      return true;
    default:
      return false;
  }
}

int codeinget() {
  int next = codein->get();
  if (next == '\n') {
    line++;
    col = 0;
  } else {
    col++;
  }
  return next;
}

int yylex() {
  int LastChar = codeinget();

  while (isspace(LastChar))
    LastChar = codeinget();

  yylloc.first_line = line;
  yylloc.first_column = col;

  if (isdigit(LastChar)) {
    std::string num;
    num += LastChar;
    while (isdigit(codein->peek()))
      num += codeinget();
    yylloc.last_line = line;
    yylloc.last_column = col;

    int NumVal;
    SPL::Util::from_string<int>(NumVal, num, std::dec);
    yylval.value = NumVal;
    return NUMBER;
  }

  // Skip multi-line comments.
  if (LastChar == '/' && codein->peek() == '*') {
    codeinget();
    while (!(codeinget() == '*' && codein->peek() == '/')) {
      ;
    }
    codeinget();
    return yylex();
  }

  // Skip single-line comments.
  if (LastChar == '/' && codein->peek() == '/') {
    codeinget();
    while (codeinget() != '\n') {
      ;
    }
    return yylex();
  }

  // String literals.
  if (LastChar == '"') {
    std::string *str = new std::string();
    while ((LastChar = codeinget()) != '"') {
      assert(LastChar != EOF); // Unexpected EOF: expected string literal terminator.
      if (LastChar == '\\') {
        switch (codeinget()) {
          case 'n': *str += "\n"; break;
          case 'r': *str += "\r"; break;
          case 't': *str += "\t"; break;
          case '"': *str += "\""; break;
        }
      } else {
        *str += LastChar;
      }
    }
    yylval.ident = str;
    return STRING;
  }

  if (LastChar == EOF)
    return -1;

  if (isaspecial(LastChar))
    return LastChar;

  // Collect until whitespace.
  std::string StrVal = "";
  StrVal += LastChar;
  // TODO: Support a+b as Plus('a', 'b'), not Name("a+b"). Same with '='.
  while (!isspace(codein->peek()) && !isaspecial(codein->peek()))
    StrVal += codeinget();
  yylloc.last_line = line;
  yylloc.last_column = col;

  if (StrVal == "def") return DEF;
  if (StrVal == "io")  return IO;
  if (StrVal == "imp") return IMP;
  if (StrVal == "extern") return EXTERN;
  if (StrVal == "var") return VAR;
  if (StrVal == "val") return TK_VAL;
  if (StrVal == "if")   return IF;
  if (StrVal == "then") return THEN;
  if (StrVal == "else") return ELSE;
  if (StrVal == "struct") return STRUCT;
  if (StrVal == "union") return UNION;
  if (StrVal == "class") return CLASS;
  if (StrVal == "instance") return INSTANCE;
  if (StrVal == "Array") return ARRAY;
  if (StrVal == "==") return EQ;
  if (StrVal == "=") return '=';
  if (StrVal == "++") return PP;
  if (StrVal == "+") return '+';

  yylval.ident = new std::string(StrVal);
  if (StrVal[0] >= 'A' && StrVal[0] <= 'Z')
    return TIDENT;
  else
    return IDENT;
}
