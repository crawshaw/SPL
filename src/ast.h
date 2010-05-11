#include "llvm/DerivedTypes.h"
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
//include "llvm/Support/IRBuilder.h"

#include <string>
#include <vector>
#include <sstream>


namespace SPL {
  namespace AST {
    enum Purity { Pure, Impure, Sealed, FunIO };

    class Expr {
    public:
      virtual llvm::Value *Codegen() = 0;
    };

    // TODO: more general literal
    class Number: public Expr {
      int Val;
    public:
      Number(int val): Val(val) {}
      virtual llvm::Value *Codegen();
    };

    class Variable : public Expr {
      std::string Name;
    public:
      Variable(const std::string &name): Name(name) {}
      virtual llvm::Value *Codegen();
    };

    class UnaryOp : public Expr {};
    class Not : public UnaryOp {
      Expr *SubExpr;
    public:
      Not(Expr &expr): SubExpr(&expr) {};
      virtual llvm::Value *Codegen();
    };

    class BinaryOp : public Expr {
      Expr *LHS, *RHS;
    public:
      BinaryOp(Expr &lhs, Expr &rhs): LHS(&lhs), RHS(&rhs) {};
    };
    class Add : public BinaryOp {
    public:
      Add(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen();
    };
    class Subtract : public BinaryOp {
    public:
      Subtract(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen();
    };
    class Multiply : public BinaryOp {
    public:
      Multiply(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen();
    };
    class Eq : public BinaryOp { // TODO replace builtin == with Eq typeclass
    public:
      Eq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen();
    };

    class Seq : public BinaryOp {
    public:
      Seq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen();
    };

    class Val : public Expr {
      std::string Name;
      Expr* Body;
    public:
      Val(const std::string &name, Expr& body): Name(name), Body(&body) {}
      virtual llvm::Value *Codegen();
    };

    class If : public Expr {
      Expr *Cond, *Then, *Else;
    public:
      If(Expr& cond, Expr& then, Expr& el)
        : Cond(&cond), Then(&then), Else(&el) {}
      virtual llvm::Value *Codegen();
    };

    class Call : public Expr {
      std::string Callee;
      std::vector<Expr*> Args;
    public:
      Call(const std::string &callee, const std::vector<Expr*> &args)
        : Callee(callee), Args(args) {}
      virtual llvm::Value *Codegen();
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
      virtual llvm::Value *Codegen();
    };

    class File {
      std::string Name;
      std::vector<Function*> Functions;
    public:
      File(const std::vector<Function*> &funcs): Functions(funcs) {}
      virtual llvm::Value *Codegen();
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

