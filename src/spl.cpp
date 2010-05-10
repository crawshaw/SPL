//include "llvm/"
#include <cstdlib>
#include <string>
#include <vector>

namespace SPL {
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

    class Parser {
      FILE* Stream;
      int LastChar;
      std::string IdentifierStr;

    public:
      Parser(FILE* stream): Stream(stream), LastChar(' ') {}

      int Next() {
        while (isspace(LastChar))
          LastChar = getc(Stream);

        if (isalpha(LastChar)) {
          IdentifierStr = LastChar;
          while (isalnum((LastChar = getc(Stream))))
            IdentifierStr += LastChar;

          if (IdentifierStr == "def") return tok_def;
          if (IdentifierStr == "io")  return tok_io;
          if (IdentifierStr == "imp") return tok_imp;
          if (IdentifierStr == "var") return tok_var;
          if (IdentifierStr == "val") return tok_val;
          return tok_identifier;
        }

        if (LastChar == EOF)
          return tok_eof;

        int ThisChar = LastChar;
        LastChar = getc(Stream);
        return ThisChar;
      }
    };
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

    class UnaryOp : public Expr {
    };

    class BinaryOp : public Expr {
    };

    class Call : public Expr {
    };

    class Function : public Expr {
      std::string Name;
      std::vector<std::string> Args;

    public:
      Function(const std::string &name, const std::vector<std::string> &args)
        : Name(name), Args(args) {}
    };

  };

};
