#include "llvm/DerivedTypes.h"
#include "llvm/Support/IRBuilder.h"

#include <string>
#include <vector>
#include <set>
#include <map>
#include <sstream>

namespace SPL {
  namespace AST {
    using namespace llvm;

    enum Purity { Pure, Impure, Sealed, FunIO };

    class Func;

    class Expr {
    public:
      virtual llvm::Value *Codegen() = 0;
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncs);
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
      virtual llvm::Type const *getType();
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
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
    };

    class UnaryOp : public Expr {
    protected:
      Expr *SubExpr;
      UnaryOp(Expr &expr): SubExpr(&expr) {};
    public:
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
    };
    class Not : public UnaryOp {
    public:
      Not(Expr &expr): UnaryOp(expr) {};
      virtual llvm::Value *Codegen();
    };

    class BinaryOp : public Expr {
    protected:
      Expr *LHS, *RHS;
      BinaryOp(Expr &lhs, Expr &rhs): LHS(&lhs), RHS(&rhs) {};
    public:
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
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

    class Bind: public Expr {
      std::string Name;
      Expr* Init;
      Expr* Body;
    public:
      Bind(const std::string &name, Expr& init, Expr& body)
        : Name(name), Init(&init), Body(&body) {}
      virtual llvm::Value *Codegen();
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
      virtual llvm::Type const *getType();
    };

    class If : public Expr {
      Expr *Cond, *Then, *Else;
    public:
      If(Expr& cond, Expr& then, Expr& el)
        : Cond(&cond), Then(&then), Else(&el) {}
      virtual llvm::Value *Codegen();
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
      virtual llvm::Type const *getType();
    };

    class Call : public Expr {
      std::string CalleeName;
      std::vector<Expr*> Args;
    public:
      Call(const std::string &calleeName, const std::vector<Expr*> &args)
        : CalleeName(calleeName), Args(args) {}
      virtual llvm::Value *Codegen();
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
    };

    class Func: public Expr {
      std::string Name;
      std::vector<std::string> Args;
      Expr* Body;
      Expr* Context;
      Purity Pureness;
      llvm::Function *function;

    std::vector<AllocaInst*> *createArgAllocas();

    public:
      Func(const std::string &name, const std::vector<std::string> &args,
        Expr &body, Expr *context, Purity purity):
        Name(name), Args(args), Body(&body), Context(context),
        Pureness(purity) {}
      std::string GetName() { return Name; }
      void setContext(Expr &context) { Context = &context; }
      virtual llvm::Value *Codegen();
      virtual std::set<std::string> *FindFreeVars(std::set<std::string> *b);
      virtual Expr* LambdaLift(std::vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
      std::string getName() { return Name; }
      llvm::Function *getFunction();
      std::vector<std::string> &getArgNames() { return Args; }
    };

    class Closure: public Expr {
      std::string FuncName;
      std::map<std::string, std::string> ActivationRecord;
      Func *FuncRef;
    public:
      Closure(const std::string &name,
        const std::map<std::string, std::string> &record, Func *func):
        FuncName(name), ActivationRecord(record), FuncRef(func) {}
      virtual llvm::Value *Codegen();
      virtual llvm::Type const *getType();
      virtual void RewriteBinding(std::string &OldName, std::string &NewName);
      Value *GenCallWith(std::vector<Value*> &args);
    };

    class File {
      std::string Name;
      std::vector<Func*> Funcs;
    public:
      File(std::string &name, const std::vector<Func*> &funcs)
        : Name(name), Funcs(funcs) {}
      void LambdaLiftFuncs();
      virtual llvm::Value *Codegen();
      void run();
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

