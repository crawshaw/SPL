#include "llvm/DerivedTypes.h"
#include "llvm/Support/IRBuilder.h"

#include <string>
#include <vector>
#include <set>
#include <map>
#include <sstream>

namespace SPL {
  namespace AST {
    using llvm::AllocaInst;
    using llvm::Value;
    using std::map;
    using std::vector;
    using std::string;

    enum Purity { Pure, Impure, Sealed, FunIO };

    class Func;

    class Expr {
    public:
      virtual llvm::Value *Codegen(map<string, Expr*> &) = 0;
      virtual Expr* LambdaLift(vector<Func*> &newFuncs);
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual llvm::Type const *getType();
    };

    // TODO: more general literal
    class Number: public Expr {
      int Val;
    public:
      Number(int val): Val(val) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual llvm::Type const *getType();
    };

    class Variable : public Expr {
      string Name;
    public:
      Variable(const string &name): Name(name) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual void RewriteBinding(string &OldName, string &NewName);
    };

    class UnaryOp : public Expr {
    protected:
      Expr *SubExpr;
      UnaryOp(Expr &expr): SubExpr(&expr) {};
    public:
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
    };
    class Not : public UnaryOp {
    public:
      Not(Expr &expr): UnaryOp(expr) {};
      virtual llvm::Value *Codegen(map<string, Expr*> &);
    };

    class BinaryOp : public Expr {
    protected:
      Expr *LHS, *RHS;
      BinaryOp(Expr &lhs, Expr &rhs): LHS(&lhs), RHS(&rhs) {};
    public:
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual llvm::Type const *getType();
    };
    class Add : public BinaryOp {
    public:
      Add(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
    };
    class Subtract : public BinaryOp {
    public:
      Subtract(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
    };
    class Multiply : public BinaryOp {
    public:
      Multiply(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
    };
    class Eq : public BinaryOp { // TODO replace builtin == with Eq typeclass
    public:
      Eq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual llvm::Type const *getType();
    };

    class Seq : public BinaryOp {
    public:
      Seq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
    };

    class Bind: public Expr {
      string Name;
      Expr* Init;
      Expr* Body;
    public:
      Bind(const string &name, Expr& init, Expr& body)
        : Name(name), Init(&init), Body(&body) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual llvm::Type const *getType();
    };

    class If : public Expr {
      Expr *Cond, *Then, *Else;
    public:
      If(Expr& cond, Expr& then, Expr& el)
        : Cond(&cond), Then(&then), Else(&el) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual llvm::Type const *getType();
    };

    class Call : public Expr {
      string CalleeName;
      vector<Expr*> Args;
    public:
      Call(const string &calleeName, const vector<Expr*> &args)
        : CalleeName(calleeName), Args(args) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
    };

    class Func: public Expr {
      string Name;
      vector<string> Args;
      vector<string*> ArgSTypes;
      const string *RetSType;
      Expr* Body;
      Expr* Context;
      Purity Pureness;
      llvm::Function *function;

    vector<AllocaInst*> *createArgAllocas();

    public:
      Func(const string &name,
          const vector<std::pair<string,string*>*> &args,
          const string* retSType,
          Expr &body, Expr *context, Purity purity):
          Name(name), Body(&body), Context(context),
          RetSType(retSType), Pureness(purity) {
        for (unsigned i=0, e=args.size(); i != e; ++i) {
          Args.push_back(args[i]->first);
          ArgSTypes.push_back(args[i]->second);
        }
      }
      Func(const string &name,
          const vector<string> &args,
          const vector<string*> &argSTypes,
          Expr &body, Expr *context, Purity purity):
          Name(name), Args(args), ArgSTypes(argSTypes),
          Body(&body), Context(context),
          Pureness(purity) {}
      string GetName() { return Name; }
      void setContext(Expr &context) { Context = &context; }
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual std::set<string> *FindFreeVars(std::set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      string getName() { return Name; }
      llvm::Function *getFunction();
      vector<string> &getArgNames() { return Args; }
    };

    class Closure: public Expr {
      string FuncName;
      map<string, string> ActivationRecord;
      Func *FuncRef;
    public:
      Closure(const string &name,
        const map<string, string> &record, Func *func):
        FuncName(name), ActivationRecord(record), FuncRef(func) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
      virtual llvm::Type const *getType();
      virtual void RewriteBinding(string &OldName, string &NewName);
      Value *GenCallWith(
        vector<Value*> &args, map<string, Expr*> &NamedExprs);
    };

    // Used to wrap a local LLVM register.
    class Register : public Expr {
      AllocaInst *Alloca;
    public:
      Register(AllocaInst *alloca): Alloca(alloca) {}
      virtual llvm::Value *Codegen(map<string, Expr*> &);
    };

    class File {
      string Name;
      vector<Func*> Funcs;
    public:
      File(string &name, const vector<Func*> &funcs)
        : Name(name), Funcs(funcs) {}
      void LambdaLiftFuncs();
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

