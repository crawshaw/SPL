//include "llvm/"
#include <cstdlib>
#include <cassert>
#include <string>
#include <sstream>
#include <iostream>
#include <vector>


namespace SPL {
  namespace Util {
    template <class T> bool from_string(T& t, const std::string& s, 
        std::ios_base& (*f)(std::ios_base&)) {
      std::istringstream iss(s);
      return !(iss >> f >> t).fail();
    }
  };

  namespace AST {

    class Expr {
    };

    // TODO: more general literal
    class Number: public Expr {
      int Val;
    public:
      Number(int val): Val(val) {}
    };

    class Variable : public Expr {
      std::string Name;
    public:
      Variable(const std::string &name): Name(name) {}
    };

    class UnaryOp : public Expr {};
    class Not : public UnaryOp {
      Expr *SubExpr;
    public:
      Not(Expr &expr): SubExpr(&expr) {};
    };

    class BinaryOp : public Expr {
      Expr *LHS, *RHS;
    public:
      BinaryOp(Expr &lhs, Expr &rhs): LHS(&lhs), RHS(&rhs) {};
    };
    class Add : public BinaryOp {
    public:
      Add(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
    };
    class Subtract : public BinaryOp {
    public:
      Subtract(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
    };
    class Multiply : public BinaryOp {
    public:
      Multiply(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
    };

    class Call : public Expr {
      std::string Callee;
      std::vector<Expr*> Args;
    public:
      Call(const std::string &callee, const std::vector<Expr*> &args)
        : Callee(callee), Args(args) {}
    };

    class Function : public Expr {
      std::string Name;
      std::vector<std::string> Args;
      std::vector<Expr*> Body;

    public:
      Function(const std::string &name, const std::vector<std::string> &args,
        const std::vector<Expr*> body) : Name(name), Args(args), Body(body) {}
    };

    class File {
      std::string Name;
      std::vector<Function*> Functions;
    public:
      File(const std::vector<Function*> &funcs): Functions(funcs) {}
    };

  };

  namespace Parser {
    enum Token {
      tok_eof         = -1,
      tok_def         = -2,
      tok_io          = -3,
      tok_imp         = -4,
      tok_var         = -5,
      tok_val         = -6,
      tok_binary      = -7,
      tok_unary       = -8,
      tok_identifier  = -9,
      tok_number      = -10
    };

    class Lexer {
      FILE* Stream;
      int LastChar;
      std::string StrVal;
      int NumVal;

    public:
      Lexer(FILE* stream): Stream(stream), LastChar(' ') {}

      int Next() {
        while (isspace(LastChar))
          LastChar = getc(Stream);

        if (isalpha(LastChar)) {
          StrVal = LastChar;
          while (isalnum((LastChar = getc(Stream))))
            StrVal += LastChar;

          if (StrVal == "def") return tok_def;
          if (StrVal == "io")  return tok_io;
          if (StrVal == "imp") return tok_imp;
          if (StrVal == "var") return tok_var;
          if (StrVal == "val") return tok_val;
          return tok_identifier;
        }

        if (isdigit(LastChar)) {
          std::string num;
          do {
            num += LastChar;
            LastChar = getc(Stream);
          } while (isdigit(LastChar));
          Util::from_string<int>(NumVal, num, std::dec);
          return tok_number;
        }

        if (LastChar == EOF)
          return tok_eof;

        int ThisChar = LastChar;
        LastChar = getc(Stream);
        return ThisChar;
      }

      std::string Identifier() { return StrVal; }
      int Number() { return NumVal; }
    };

    class Parser {
      Lexer *Lex;
      int CurTok;

      void next() {
        CurTok = Lex->Next();
      }

      std::vector<std::string> parseArgs() {
        std::vector<std::string> args;
        assert(CurTok == '(');
        next();
        while (1) {
          switch (CurTok) {
            case ')': return args;
            case ',':
              next();
              break;
            case tok_identifier:
              args.push_back(Lex->Identifier());
              next();
              break;
            default:
              assert(false); // Expected argument name.
          }
        }
      }

      AST::Expr* parseExpr() {
        switch (CurTok) {
          case tok_number: new AST::Number(Lex->Number());
        }
      }

      std::vector<AST::Expr*> parseExprBlock() {
        std::vector<AST::Expr*> exprs;
        assert(CurTok == '{');
        next();
        while (1) {
          exprs.push_back(parseExpr());
          assert(CurTok == ';');
          next();
          if (CurTok == '}')
            return exprs;
        }
      }

      AST::Function* parseFunction() {
        int FunctionType = CurTok;
        next();
        std::string name = Lex->Identifier();

        next();
        std::vector<std::string> args = parseArgs();
        assert(CurTok == '=');
        next();
        std::vector<AST::Expr*> exprs = parseExprBlock();

        new AST::Function(name, args, exprs);
      }

    public:
      Parser(Lexer *lex): Lex(lex) {}

      AST::File *parseFile() {
        std::vector<AST::Function*> funcs;

        next();
        while (CurTok != tok_eof) {
          funcs.push_back(parseFunction());
        }

        return new AST::File(funcs);
      }
    };
  };
};


int main(void) {
  SPL::Parser::Lexer *lexer = new SPL::Parser::Lexer(stdin);
  SPL::Parser::Parser *parser = new SPL::Parser::Parser(lexer);

  return 0;
}

