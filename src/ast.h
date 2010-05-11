#include <string>
#include <vector>
#include <sstream>

namespace SPL {
  namespace AST {
    enum Purity { Pure, Impure, Sealed, FunIO };

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

    class Seq : public BinaryOp {
    public:
      Seq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
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
      Expr* Body;
      Purity Pureness;

    public:
      Function(const std::string &name, const std::vector<std::string> &args,
        Expr &body, Purity purity):
        Name(name), Args(args), Body(&body), Pureness(purity) {}
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
  };

  namespace Util {
    template <class T> bool from_string(T& t, const std::string& s, 
        std::ios_base& (*f)(std::ios_base&)) {
      std::istringstream iss(s);
      return !(iss >> f >> t).fail();
    }
  };


};

