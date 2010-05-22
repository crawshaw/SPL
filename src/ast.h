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
    using std::multimap;
    using std::vector;
    using std::string;
    using std::pair;
    using std::set;

    enum Purity { Pure, Impure, Sealed, FunIO };

    class Expr;
    class Func;
    class Member;
    class SType;
    class SStructType;
    class SFunctionType;

    class TypeInferer {
      multimap<Expr*, Expr*>  eqns;
      map<Expr*, SType*>      tys;
      vector<Member*>         members;
      unsigned ResolveMembers();
    public:
      void TypeUnification();
      void TypePopulation();
      void eqn(Expr* lhs, Expr* rhs);
      void ty(Expr* expr, SType* ty);
      void member(Member *);
    };

    class Expr {
    protected:
      SType *ThisType;
      Expr(): ThisType(NULL) {}
    public:
      virtual void Bind(map<string, Expr*> &) = 0;
      virtual Value *Codegen() = 0;
      virtual Expr* LambdaLift(vector<Func*> &newFuncs);
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual void TypeInfer(TypeInferer &) = 0;
      virtual SType *getSType() { return ThisType; }
      virtual void setSType(SType *ty) { ThisType = ty; }
      virtual Type const *getType();
    };

    // TODO: more general literal
    class Number: public Expr {
      int Val;
    public:
      Number(int val): Val(val) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      //virtual Type const *getType();
    };

    class Variable : public Expr {
      string Name;
      Expr *Binding;
    public:
      Variable(const string &name): Name(name) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
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
      virtual void TypeInfer(TypeInferer &);
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
      virtual void TypeInfer(TypeInferer &);
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      //virtual Type const *getType();
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
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      //virtual Type const *getType();
    };

    class Seq : public BinaryOp {
    public:
      Seq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    class Member : public Expr {
      Expr *Source;
      const string FieldName;
      SStructType *getSourceSType();
    public:
      Member(Expr &source, string &fieldName)
        : Source(&source), FieldName(fieldName) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void TypeInferSecondPass();
      virtual Value *Codegen();
      Expr *getSource() { return Source; }
      //virtual Type const *getType();
    };

    class Register;

    class Binding: public Expr {
      string Name;
      Expr* Init;
      Register *InitReg;
      Expr* Body;
    public:
      Binding(const string &name, Expr& init, Expr& body)
        : Name(name), Init(&init), InitReg(NULL), Body(&body) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      //virtual Type const *getType();
    };

    class If : public Expr {
      Expr *Cond, *Then, *Else;
    public:
      If(Expr& cond, Expr& then, Expr& el)
        : Cond(&cond), Then(&then), Else(&el) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      //virtual Type const *getType();
    };

    class Call : public Expr {
      string CalleeName;
      Expr *Callee; // Either a Closure or a Func. TODO: Subclass Call?
      vector<Expr*> Args;
    public:
      Call(const string &calleeName, const vector<Expr*> &args)
        : CalleeName(calleeName), Args(args) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual SType *getSType();
    };

    // Used to wrap a local LLVM register.
    class Register : public Expr {
      const string Name;
      //AllocaInst *Alloca;
      Value *Alloca;
      Expr *Source;
    public:
      Register(string &name, Expr *expr)
        : Name(name), Source(expr), Alloca(NULL) { }
      virtual void Bind(map<string, Expr*> &) {}
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    // Used to wrap a local LLVM register for a function argument.
    class RegisterFunArg : public Expr {
      AllocaInst *Alloca;
    public:
      RegisterFunArg(SType *ty): Alloca(NULL) { ThisType = ty; }
      void setAlloca(AllocaInst *a) { Alloca = a; }
      virtual void Bind(map<string, Expr*> &) {}
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    class Func: public Expr {
      string Name;
      vector<string> Args;
      vector<string*> ArgSTypeNames;
      vector<SType*> ArgSTypes; // TODO: redundant, just use ThisType.
      vector<RegisterFunArg*> ArgRegs;
      const string *RetSTypeName;
      SType *RetSType;
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
          RetSTypeName(retSType), RetSType(NULL),
          function(NULL), Pureness(purity) {
        for (unsigned i=0, e=args.size(); i != e; ++i) {
          Args.push_back(args[i]->first);
          ArgSTypeNames.push_back(args[i]->second);
        }
      }
      Func(const string &name,
          const vector<string> &args,
          const vector<string*> &argSTypes,
          const string* retSType,
          Expr &body, Expr *context, Purity purity):
          Name(name), Args(args), ArgSTypeNames(argSTypes),
          Body(&body), Context(context),
          RetSTypeName(retSType), RetSType(NULL),
          function(NULL), Pureness(purity) {}
      const string GetName() { return Name; }
      void setContext(Expr &context) { Context = &context; }
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      SFunctionType *getFunctionSType();
      string getName() { return Name; }
      Function *getFunction();
      vector<string> &getArgNames() { return Args; }
      void getArgRegs(vector<RegisterFunArg*> &args) {
        args.insert(args.end(), ArgRegs.begin(), ArgRegs.end());
      }
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
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      //virtual Type const *getType();
      virtual void RewriteBinding(string &OldName, string &NewName);
      Func *getFunction() { return FuncRef; }
      Value *GenCallWith(vector<Value*> &args);
      void getArgRegs(vector<RegisterFunArg*> &args);
    };

    class Constructor : public Expr {
      const string STypeName;
      vector<Expr*> Args;
    public:
      Constructor(const string &stName, const vector<Expr*> &args)
        : STypeName(stName), Args(args) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    class SType {
    protected:
      const string Name;
    public:
      SType(const string &name): Name(name) {}
      virtual void Bind(vector<string> &, map<string, SType*> &) {}
      virtual Type const *getType() = 0;
      virtual Type const *getPassType() { return getType(); }
      virtual void dump() = 0;
      const string getName() { return Name; }
    };

    class SPrimitive : public SType {
    public:
      SPrimitive(const string &name): SType(name) {}
      virtual void dump();
    };

    class Int8  : public SPrimitive { public:
      Int8(): SPrimitive("Int8") {} virtual Type const *getType(); };
    class Int16 : public SPrimitive { public:
      Int16(): SPrimitive("Int16") {} virtual Type const *getType(); };
    class Int32 : public SPrimitive { public:
      Int32(): SPrimitive("Int32") {} virtual Type const *getType(); };
    class Int64 : public SPrimitive { public:
      Int64(): SPrimitive("Int64") {} virtual Type const *getType(); };
    class SBool : public SPrimitive { public:
      SBool(): SPrimitive("Bool") {} virtual Type const *getType(); };

    // TODO: common superclass for SStructType and SUnionType.

    class SStructType : public SType {
      vector<string>  ElementNames;
      vector<string>  ElementSTypeNames;
      vector<SType*>  ElementSTypes;
      Type * ThisType;
    public:
      SStructType(string &name, const vector<pair<string,string*>*> &els)
          : SType(name), ThisType(NULL) {
        for (unsigned i=0, e=els.size(); i != e; ++i) {
          ElementNames.push_back(els[i]->first);
          ElementSTypeNames.push_back(string(*els[i]->second));
        }
      }
      virtual void Bind(vector<string> &, map<string, SType*> &);
      virtual Type const *getType();
      virtual Type const *getPassType();
      virtual void dump();
      Type const *getType(int idx) { return ElementSTypes[idx]->getType(); }
      SType *getSType(int idx) { return ElementSTypes[idx]; }
      unsigned getIndex(const string &name);
      void getElementSTypes(vector<SType*> &argTys) {
        argTys.assign(ElementSTypes.begin(), ElementSTypes.end());
      }
    };

    class SFunctionType : public SType {
      vector<SType*> Args;
      SType* Ret;
    public:
      SFunctionType(string &name, vector<SType*> &args, SType* ret)
        : SType(name), Args(args), Ret(ret) {}
      vector<SType*> &getArgs() { return Args; }
      SType* getReturnType() { return Ret; }
      llvm::FunctionType const *getFunctionType();
      virtual Type const *getType();
      virtual void dump();
    };

    class SPtr : public SType {
      SType *Ref;
    public:
      SPtr(SType *ref): SType("Ptr:" + ref->getName()), Ref(ref) {}
      virtual Type const *getType();
      virtual void dump();
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

