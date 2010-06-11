#include "llvm/DerivedTypes.h"
#include "llvm/Support/IRBuilder.h"
#include "llvm/Module.h"
#include "llvm/Support/Debug.h"

#include <string>
#include <vector>
#include <set>
#include <map>
#include <sstream>

namespace SPL {
  namespace AST {
    using llvm::Module;
    using llvm::AllocaInst;
    using llvm::Value;
    using llvm::Type;
    using llvm::Function;
    using llvm::dbgs;
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
    class ArrayAccess;
    class Call;
    class SType;
    class SStructType;
    class SFunctionType;
    class SGenericType;
    class SArray;

    class TypeInferer {
      multimap<Expr*, Expr*>  eqns;
      map<Expr*, SType*>      tys;
      vector<Member*>         members;
      vector<ArrayAccess*>    arrayAccesses;
      unsigned ResolveMembers();
      unsigned ResolveArrayAccesses();
    public:
      void TypeUnification();
      void TypePopulation();
      void eqn(Expr* lhs, Expr* rhs);
      void ty(Expr* expr, SType* ty);
      void member(Member *);
      void arrayAccess(ArrayAccess *);
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &) = 0;
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual void TypeInfer(TypeInferer &) = 0;
      virtual SType *getSType() { return ThisType; }
      virtual void setSType(SType *ty) { ThisType = ty; }
      virtual Type const *getType();
      virtual Value *LValuegen();
    };

    // TODO: more general literal
    class Number: public Expr {
      int Val;
    public:
      Number(int val): Val(val) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
      //virtual Type const *getType();
    };

    class StringLiteral: public Expr {
      const std::string Str;
    public:
      StringLiteral(std::string &s): Str(s) {}
      const std::string &get() { return Str; }
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
    };

    class Variable : public Expr {
      string Name;
      Expr *Binding;
    public:
      Variable(const string &name): Name(name) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual void RewriteBinding(string &OldName, string &NewName);
      virtual Value *LValuegen();
    };

    class UnaryOp : public Expr {
    protected:
      Expr *SubExpr;
      UnaryOp(Expr &expr): SubExpr(&expr) {};
    public:
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
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
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };
    class JoinString: public BinaryOp {
    public:
      JoinString(Expr &lhs, Expr &rhs)
        : BinaryOp(lhs, rhs) {}
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    class Seq : public BinaryOp {
    public:
      Seq(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs) {}
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    class Assign : public BinaryOp {
    public:
      Assign(Expr &lhs, Expr &rhs): BinaryOp(lhs, rhs){}
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
    };

    class ArrayAccess : public BinaryOp {
      SArray *getSourceSType();
    public:
      ArrayAccess(Expr &lhs, Expr &rhs): BinaryOp(lhs,rhs) {}
      virtual void TypeInfer(TypeInferer &);
      virtual void TypeInferSecondPass();
      virtual Value *Codegen();
      virtual Value *LValuegen();
      Expr *getSource() { return LHS; }
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual void TypeInferSecondPass();
      virtual Value *Codegen();
      virtual Value *LValuegen();
      Expr *getSource() { return Source; }
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
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
      void getAllArgs(vector<Expr*> &);

    public:
      Call(const string &calleeName, const vector<Expr*> &args)
        : CalleeName(calleeName), Args(args) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      Func* getFunc();

      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      void getGenerics(vector<SType*> &);
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
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
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
    };

    // TODO:  we could at least have Func subclass
    //        Closure, to avoid some dynamic_cast
    class Func: public Expr {
      string Name;
      vector<string>          Generics;
      vector<SGenericType*>   GenericPlaceholders;
      bool GenericsAreBound;
      vector<string> Args;
      vector<string*> ArgSTypeNames;
      vector<SType*> ArgSTypes; // TODO: redundant, just use ThisType.
      vector<RegisterFunArg*> ArgRegs;
      const string *RetSTypeName;
      SType *RetSType;
      Expr* Body;
      Expr* Context; // Only valid prior to lambda lifting.
      Purity Pureness;
      map<vector<SType*>,Function*> functions;

    vector<AllocaInst*> *createArgAllocas();

    public:
      // XXX what a mess.
      Func(const string &name,
          const vector<string> &argstys,
          const string &retsty):
          Name(name), GenericsAreBound(false),
          RetSTypeName(&retsty), RetSType(NULL),
          Body(NULL), Context(NULL),
          Pureness(FunIO) {
        for (unsigned i=0, e=argstys.size(); i!=e; ++i){
          Args.push_back("$");
          ArgSTypeNames.push_back(new string(argstys[i]));
        }
      }

      Func(const string &name,
          const vector<string> &generics,
          const vector<pair<string,string*>*> &args,
          const string* retSType,
          Expr &body, Expr *context, Purity purity):
          Name(name), Body(&body), Context(context),
          RetSTypeName(retSType), RetSType(NULL),
          Pureness(purity),
          Generics(generics), GenericsAreBound(false) {
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
          Pureness(purity) {}
      const string GetName() { return Name; }
      void setContext(Expr &context) { Context = &context; }
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
      virtual set<string> *FindFreeVars(set<string> *b);
      virtual Expr* LambdaLift(vector<Func*> &newFuncsnewFuncsnewFuncs);
      virtual void RewriteBinding(string &OldName, string &NewName);
      SFunctionType *getFunctionSType();
      void getFullName(string &fnName);
      virtual Function *getFunction();
      vector<string> &getArgNames() { return Args; }
      void getArgRegs(vector<RegisterFunArg*> &args) {
        args.insert(args.end(), ArgRegs.begin(), ArgRegs.end());
      }
      void getGenerics(vector<SType*> &gen);
      bool isGeneric();
      void setGenerics(const vector<SType*> &tys);
      void clearGenerics();
      void MatchGenerics(const vector<SType*> &callTypes,
          vector<SType*> &genericBindings);
    };

    class Extern: public Func {
    public:
      Extern(
          const string &name, 
          const vector<string> &args,
          const string &retSType):
          Func(name, args, retSType) {}

      virtual Function *getFunction();
      virtual Value *Codegen() { return getFunction(); }
    };

    class Closure: public Expr {
      string FuncName;
      vector<string> ActivationRecordNames;
      vector<Expr*> ActivationRecord;
      Func *FuncRef;
    public:
      Closure(const string &name,
        const vector<string> &record, Func *func):
        FuncName(name), ActivationRecordNames(record), FuncRef(func) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
      //virtual Type const *getType();
      virtual void RewriteBinding(string &OldName, string &NewName);
      Func *getFunc() { return FuncRef; }
      void getArgRegs(vector<RegisterFunArg*> &args);
      vector<Expr*> *getActivationRecord();
    };

    class Array : public Expr {
      const string STypeName;
      SType *Contained;
      Expr *SizeExpr;
    public:
      Array(const string &stName, Expr &sizeExpr):
        STypeName(stName), SizeExpr(&sizeExpr) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
    };

    class Constructor : public Expr {
      const string STypeName;
      vector<Expr*> Args;
    public:
      Constructor(
          const string &stName,
          const vector<Expr*> &args)
        : STypeName(stName), Args(args) {}
      virtual void Bind(map<string, Expr*> &);
      virtual void TypeInfer(TypeInferer &);
      virtual void FindCalls(vector<pair<Func*,vector<SType*> > > &);
      virtual Value *Codegen();
    };

    class SType {
      static map<string,SType*> BuiltinsMap;
    protected:
      const string Name;
    public:
      SType(const string &name): Name(name) {}
      virtual void Bind(vector<string> &, const map<string, SType*> &) {}
      virtual Type const *getType() = 0;
      virtual Type const *getPassType() { return getType(); }
      virtual void dump() = 0;
      const string getName() { return Name; }

      static const map<string,SType*> &Builtins();
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
    protected:
      vector<string>  ElementNames;
      vector<string>  ElementSTypeNames;
      vector<SType*>  ElementSTypes;
      Type * ThisType;
    public:
      SStructType(const string &name): SType(name), ThisType(NULL) {}
      SStructType(const string &name, const vector<pair<string,string*>*> &els)
          : SType(name), ThisType(NULL) {
        for (unsigned i=0, e=els.size(); i != e; ++i) {
          ElementNames.push_back(els[i]->first);
          ElementSTypeNames.push_back(string(*els[i]->second));
        }
      }
      virtual void Bind(vector<string> &, const map<string, SType*> &);
      virtual Type const *getType();
      virtual Type const *getPassType();
      virtual void dump();
      Type const *getType(int idx) { return ElementSTypes[idx]->getType(); }
      SType *getSType(int idx) { return ElementSTypes[idx]; }
      unsigned getIndex(const string &name);
      void getElementSTypes(vector<SType*> &argTys) {
        argTys.assign(ElementSTypes.begin(), ElementSTypes.end());
      }
      bool isUnboxed() { return false; /* TODO */ }
    };

    // As these are general-purpose arrays, their size
    // is not known until runtime. Therefore we must
    // represent them with as an llvm::StructType
    // containing an llvm::ArrayType with NumElements=0
    // and an Int32 containing the size.
    //
    // TODO:  this struct-with-array double pointer
    //        is conceptually simple but an unnecessary
    //        cache hit that can be compiled out by
    //        either hiding the size in or 'behind' the
    //        first element. Learn more about LLVM
    //        before doing this.
    class SArray : public SStructType {
      SType *Contained;
    public:
      SArray(SType *);
      SType *getContained() { return Contained; }
      virtual void Bind(vector<string> &, const map<string, SType*> &);
      virtual void dump();
    };

    // Stored as { i32, [ 0 x i8]* }*, with the strings
    // also NULL-terminated for easy C interop.
    class SString : public SArray {
      static SString *Singleton;
      vector<string>  ElementNames;
      vector<string>  ElementSTypeNames;
      vector<SType*>  ElementSTypes;
      Type * ThisType;
    public:
      SString(): SArray(new Int8()) {}
      virtual void dump();

      static SString *get() {
        if (Singleton == NULL)
          Singleton = new SString();
        return Singleton;
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

    class SGenericType : public SType {
      unsigned id;
      SType *Binding;
    public:
      SGenericType(const string &name): SType(name), Binding(NULL) {
        static unsigned gid = 0;
        id = gid++;
      }
      virtual Type const *getType();
      virtual void dump();
      void setBinding(SType *ty) { Binding = ty; }
      SType *getBinding() { return Binding; }
    };

    class Class;
    class Instance;

    // TODO: this is really a 'program', as when we
    // do have multiple files, they will all be merged
    // into just one of these classes.
    class File {
      string Name;
      vector<Func*> Funcs;
      vector<Extern*> Externs;
      vector<SType*> STypes;
      void LambdaLiftFuncs();
      Module FileModule;

    public:
      File(string &name,
          const vector<Func*> &funcs,
          const vector<Extern*> &externs,
          const vector<SType*> &tys);
      void compile();
      void optimize();
      Module &getModule() { return FileModule; }
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

    template<typename T>
    void removeDuplicates(std::vector<T>& vec) {
      std::sort(vec.begin(), vec.end());
      vec.erase(std::unique(vec.begin(), vec.end()), vec.end());
    }

    template<typename T>
    bool allEqual(std::vector<T>& vec, T& val) {
      bool allEq = true;
      for (unsigned i=0, e=vec.size(); i != e; ++i) {
        if (vec[i] != val) {
          allEq = false;
          break;
        }
      }
      return allEq;
    }

    template<typename T>
    bool allNull(std::vector<T>& vec) {
      for (unsigned i=0, e=vec.size(); i != e; ++i)
        if (vec[i] != NULL)
          return false;
      return true;
    }

    template<typename T>
    bool contains(std::vector<T>& vec, T& val) {
      for (unsigned i=0, e=vec.size(); i != e; ++i)
        if (vec[i] == val)
          return true;
      return false;
    }
  };


};

