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
    using llvm::Type;
    using llvm::Function;
    using std::map;
    using std::vector;
    using std::string;
    using std::pair;
    using std::set;

    enum Purity { Pure, Impure, Sealed, FunIO };

    class Func;

    class Expr {
    public:
      virtual void Bind(map<string, Expr*> &) = 0;
      virtual Value *Codegen() = 0;
      virtual Expr* LambdaLift(vector<Func*> &newFuncs);
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual Type const *getType();
    };

    // TODO: more general literal
    class Number: public Expr {
      int Val;
    public:
      Number(int val): Val(val) {}
      virtual void Bind(map<string, Expr*> &) {}
      virtual Value *Codegen();
      virtual Type const *getType();
    };

    class Variable : public Expr {
      string Name;
      Expr *Binding;
    public:
      Variable(const string &name): Name(name) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual void RewriteBinding(string &OldName, string &NewName);
    };

    class UnaryOp : public Expr {
    protected:
      Expr *SubExpr;
      UnaryOp(Expr &expr): SubExpr(&expr) {};
    public:
      virtual void Bind(map<string, Expr*> &);
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
    };
    class Not : public UnaryOp {
    public:
      Not(Expr &expr): UnaryOp(expr) {};
      virtual Value *Codegen();
    };

    class BinaryOp : public Expr {
    protected:
      Expr *LHS, *RHS;
      BinaryOp(Expr &lhs, Expr &rhs): LHS(&lhs), RHS(&rhs) {};
    public:
      virtual void Bind(map<string, Expr*> &);
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual Type const *getType();
    };
    class Add : public BinaryOp {
    public:
      Add(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual Value *Codegen();
    };
    class Subtract : public BinaryOp {
    public:
      Subtract(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual Value *Codegen();
    };
    class Multiply : public BinaryOp {
    public:
      Multiply(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual Value *Codegen();
    };
    class Eq : public BinaryOp { // TODO replace builtin == with Eq typeclass
    public:
      Eq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual Value *Codegen();
      virtual Type const *getType();
    };

    class Seq : public BinaryOp {
    public:
      Seq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual Value *Codegen();
    };

    class Member : public BinaryOp {
      // TODO: AllocaInst *Struct;
    public:
      Member(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual Type const *getType();
    };

    class Binding: public Expr {
      string Name;
      Expr* Init;
      Expr* Body;
    public:
      Binding(const string &name, Expr& init, Expr& body)
        : Name(name), Init(&init), Body(&body) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual Type const *getType();
    };

    class If : public Expr {
      Expr *Cond, *Then, *Else;
    public:
      If(Expr& cond, Expr& then, Expr& el)
        : Cond(&cond), Then(&then), Else(&el) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual Type const *getType();
    };

    class Call : public Expr {
      string CalleeName;
      Expr *Callee; // Either a Closure or a Func. TODO: Subclass Call?
      vector<Expr*> Args;
    public:
      Call(const string &calleeName, const vector<Expr*> &args)
        : CalleeName(calleeName), Args(args) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
    };

    // Used to wrap a local LLVM register.
    class Register : public Expr {
      const string Name;
      AllocaInst *Alloca;
      Expr *Source;
    public:
      Register(string &name, Expr *expr)
        : Name(name), Source(expr), Alloca(NULL) {}
      virtual void Bind(map<string, Expr*> &) {}
      virtual Value *Codegen();
    };

    // Used to wrap a local LLVM register for a function argument.
    class RegisterFunArg : public Expr {
      AllocaInst *Alloca;
    public:
      RegisterFunArg() { Alloca = NULL; }
      void setAlloca(AllocaInst *a) { Alloca = a; }
      virtual void Bind(map<string, Expr*> &) {}
      virtual Value *Codegen();
    };

    class Func: public Expr {
      string Name;
      vector<string> Args;
      vector<string*> ArgSTypes;
      vector<RegisterFunArg*> ArgRegs;
      const string *RetSType;
      Expr* Body;
      Expr* Context;
      Purity Pureness;
      Function *function;

    vector<AllocaInst*> *createArgAllocas();

    public:
      Func(const string &name,
          const vector<pair<string,string*>*> &args,
          const string* retSType,
          Expr &body, Expr *context, Purity purity):
          Name(name), Body(&body), Context(context),
          RetSType(retSType), function(NULL), Pureness(purity) {
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
          function(NULL),
          Pureness(purity) {}
      const string GetName() { return Name; }
      void setContext(Expr &context) { Context = &context; }
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      string getName() { return Name; }
      Function *getFunction();
      vector<string> &getArgNames() { return Args; }
    };

    class Closure: public Expr {
      string FuncName;
      map<string, string> ActivationRecordNames;
      map<string, Expr*> ActivationRecord;
      Func *FuncRef;
    public:
      Closure(const string &name,
        const map<string, string> &record, Func *func):
        FuncName(name), ActivationRecordNames(record), FuncRef(func) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
      virtual Type const *getType();
      virtual void RewriteBinding(string &OldName, string &NewName);
      Value *GenCallWith(vector<Value*> &args);
    };

    class SType;
    class SStructType;

    class Constructor : public Expr {
      const string STypeName;
      SStructType *SType;
      vector<Expr*> Args;
    public:
      Constructor(const string &stName, const vector<Expr*> &args)
        : STypeName(stName), Args(args), SType(NULL) {}
      virtual void Bind(map<string, Expr*> &);
      virtual Value *Codegen();
    };

    class SType {
    };

    class SStructType : public SType {
      string Name;
      vector<string>  ElementNames;
      vector<string>  ElementSTypeNames;
      vector<SType>   ElementSTypes;
    public:
      SStructType(string &name, const vector<pair<string,string*>*> &els)
          : Name(name) {
        for (unsigned i=0, e=els.size(); i != e; ++i) {
          ElementNames.push_back(els[i]->first);
          ElementSTypeNames.push_back(*els[i]->second);
        }
      }
    };

    class SUnionType  : public SType {
    };

    class Class;
    class Instance;

    class File {
      string Name;
      vector<Func*> Funcs;
      vector<SType*> STypes;
    public:
      File(string &name, const vector<Func*> &funcs, const vector<SType*> &tys)
        : Name(name), Funcs(funcs), STypes(tys) {}
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

