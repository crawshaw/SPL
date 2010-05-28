#include "ast.h"

#include "llvm/LLVMContext.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Target/TargetData.h"
#include "llvm/Target/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/IRBuilder.h"

#include <iostream>
#include <map>

using std::map;
using std::vector;
using std::string;
using namespace llvm;

static IRBuilder<> Builder(getGlobalContext());

namespace SPL { namespace AST {

// TODO: not globals
static Module *TheModule;
static FunctionPassManager *TheFPM;
static ExecutionEngine *TheExecutionEngine;

// TODO: pass to Bind() along with NamedExprs.
map<string,SType*> NamedTypes;


/////////////////////////////////////////////////////////////////////


Type const *Expr::getType() { return ThisType->getType(); }

SFunctionType *Func::getFunctionSType() {
  return dynamic_cast<SFunctionType*>(ThisType); // XXX yuck.
}
FunctionType const *SFunctionType::getFunctionType() {
  vector<const Type*> ArgTypes;
  for (vector<SType*>::const_iterator i=Args.begin(); i!=Args.end(); i++)
    ArgTypes.push_back((*i)->getType());
  return FunctionType::get(Ret->getType(), ArgTypes, false);
}

SStructType *Member::getSourceSType() {
  SStructType *sty = dynamic_cast<SStructType*>(Source->getSType());
  if (sty == NULL) {
    std::cerr << "Called member field `" << FieldName
      << "'on non-structure type: ";
    Source->getSType()->dump();
    std::cerr << std::endl;
    exit(1);
  }
  return sty;
}

/////////////////////////////////////////////////////////////////////


void Number::Bind(map<string, Expr*> &) {
}

void Variable::Bind(map<string, Expr*> &NamedExprs) {
  Binding = NamedExprs[Name];
  if (Binding == NULL) {
    std::cerr << "Failed to bind " << Name << std::endl;
    exit(1);
  }
}

void UnaryOp::Bind(map<string, Expr*> &NamedExprs) {
  SubExpr->Bind(NamedExprs);
}

void BinaryOp::Bind(map<string, Expr*> &NamedExprs) {
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
}

void Eq::Bind(map<string, Expr*> &NamedExprs) {
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
}

void Member::Bind(map<string, Expr*> &NamedExprs) {
  Source->Bind(NamedExprs);
}

void Constructor::Bind(map<string, Expr*> &NamedExprs) {
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->Bind(NamedExprs);
}

void Binding::Bind(map<string, Expr*> &NamedExprs) {
  Init->Bind(NamedExprs);

  Expr *OldExpr = NamedExprs[Name];
  InitReg = new Register(Name, Init);
  InitReg->Bind(NamedExprs);
  NamedExprs[Name] = InitReg;

  Body->Bind(NamedExprs);

  NamedExprs[Name] = OldExpr;
}

void If::Bind(map<string, Expr*> &NamedExprs) {
  Cond->Bind(NamedExprs);
  Then->Bind(NamedExprs);
  Else->Bind(NamedExprs);
}

void Closure::Bind(map<string,Expr*> &NamedExprs) {
  vector<string>::const_iterator it;
  for (it=ActivationRecordNames.begin(); it!=ActivationRecordNames.end(); ++it)
    ActivationRecord.push_back(NamedExprs[*it]);
}

void Call::Bind(map<string, Expr*> &NamedExprs) {
  Callee = NamedExprs[CalleeName];

  if (Callee == NULL) {
    std::cerr << "Cannot find function `" << CalleeName << "'" << std::endl;
    exit(1);
  }

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->Bind(NamedExprs);
}

void Func::Bind(map<string, Expr*> &NamedExprs) {
  // For now we are using local type inference, this means that each
  // function definition must have a type signature. This is overly
  // restrictive, I believe our type system can be fully inferred.
  // But it means we can resolve the function type right here, and
  // then rely on Func types in the inference stage.

  for (unsigned i=0, e=Generics.size(); i != e; ++i)
    NamedTypes[Generics[i]] = new SGenericType(Generics[i]);

  RetSType = NamedTypes[*RetSTypeName];
  if (RetSType == NULL) {
    std::cerr << "Unknown return type `" << *RetSTypeName <<
      "' on function `" << Name << "'." << std::endl;
    exit(1);
  }

  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    SType *ty = NamedTypes[*ArgSTypeNames[i]];
    if (ty == NULL) {
      std::cerr << "Unknown argument type `" << ArgSTypeNames[i] <<
        "' on function `" << Name << "'." << std::endl;
      exit(1);
    }
    ArgSTypes.push_back(ty);
  }

  for (unsigned i=0, e=Generics.size(); i != e; ++i)
    NamedTypes[Generics[i]] = NULL;

  setSType(new SFunctionType(Name, ArgSTypes, RetSType));

  // Bind body.
  vector<Expr*> oldBindings;
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    RegisterFunArg *reg = new RegisterFunArg(ArgSTypes[i]);
    ArgRegs.push_back(reg);
    oldBindings.push_back(
      NamedExprs.count(Args[i]) == 0 ? NULL : NamedExprs[Args[i]]);
    NamedExprs[Args[i]] = reg;
  }

  Body->Bind(NamedExprs);

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    NamedExprs[Args[i]] = oldBindings[i];
}


/////////////////////////////////////////////////////////////////////


void Number::FindCalls(vector<Call*> &calls) { }
void Variable::FindCalls(vector<Call*> &calls) { }
void UnaryOp::FindCalls(vector<Call*> &calls) {
  SubExpr->FindCalls(calls);
}
void BinaryOp::FindCalls(vector<Call*> &calls) {
  LHS->FindCalls(calls);
  RHS->FindCalls(calls);
}
void Member::FindCalls(vector<Call*> &calls) {
  Source->FindCalls(calls);
}
void Binding::FindCalls(vector<Call*> &calls) {
  Init->FindCalls(calls);
  Body->FindCalls(calls);
}
void If::FindCalls(vector<Call*> &calls) {
  Cond->FindCalls(calls);
  Then->FindCalls(calls);
  Else->FindCalls(calls);
}
void Call::FindCalls(vector<Call*> &calls) {
  calls.push_back(this);
}
void Func::FindCalls(vector<Call*> &calls) {
  Body->FindCalls(calls);
}
void Closure::FindCalls(vector<Call*> &calls) {
  // TODO: what about the ActivationRecord?
}
void Constructor::FindCalls(vector<Call*> &calls) {
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->FindCalls(calls);
}
void Register::FindCalls(vector<Call*> &calls) {}
void RegisterFunArg::FindCalls(vector<Call*> &calls) {}


/////////////////////////////////////////////////////////////////////


Value *Number::Codegen() {
  return ConstantInt::get(getGlobalContext(), APInt(32, Val, true));
}

Value *Variable::Codegen() {
  Value *V = Binding->Codegen();
  if (V == 0) {
    std::cerr << "Unknown variable: `" << Name << "'" << std::endl;
    return NULL;
  }
  return Builder.CreateLoad(V, Name.c_str());
}

Value *Not::Codegen() {
  return NULL;
}
Value *Add::Codegen() {
  return Builder.CreateAdd(LHS->Codegen(), RHS->Codegen(), "addtmp");
}

Value *Subtract::Codegen() {
  return Builder.CreateSub(LHS->Codegen(), RHS->Codegen(), "subtmp");
}

Value *Multiply::Codegen() {
  return Builder.CreateMul(LHS->Codegen(), RHS->Codegen(), "multmp");
}

Value *Eq::Codegen() {
  return Builder.CreateICmpEQ(LHS->Codegen(), RHS->Codegen(), "eqtmp");
}

Value *Seq::Codegen() {
  return NULL;
}

Value *Member::Codegen() {
  SStructType *sty = getSourceSType();

  unsigned fieldIndex = sty->getIndex(FieldName);
  std::cout << "fieldIndex = " << fieldIndex << std::endl;
  Type const *fieldTy = sty->getType(fieldIndex);
  std::cout << "fieldTy: ";
  fieldTy->dump();

  /*
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());
  AllocaInst *a = TmpB.CreateAlloca(fieldTy, 0, FieldName);
  */

  Value *src = Source->Codegen();
  Value *gep = Builder.CreateStructGEP(src, fieldIndex);
  return Builder.CreateLoad(gep);
}

Value *Constructor::Codegen() {
  // TODO: heap allocation.
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());
  AllocaInst *a = TmpB.CreateAlloca(
    ThisType->getPassType(), 0, STypeName.c_str());

  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    Value *gep = TmpB.CreateStructGEP(a, i);
    Value *val = Args[i]->Codegen();
    Builder.CreateStore(val, gep);
  }

  return a;
}

Value *Register::Codegen() {
  if (Alloca == NULL) {
    Value *InitVal = Source->Codegen();
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
      TheFunction->getEntryBlock().begin());
    Alloca = TmpB.CreateAlloca(Source->getType(), 0, Name.c_str());

    Builder.CreateStore(InitVal, Alloca);
  }

  return Alloca;
}

Value *RegisterFunArg::Codegen() {
  assert(Alloca != NULL);
  return Alloca;
}

Value *Binding::Codegen() {
  return Body->Codegen();
}

Value *If::Codegen() {
  Value *condVal = Cond->Codegen();
  if (condVal == NULL) return NULL;
  if (!condVal->getType()->isIntegerTy()) {
    std::cerr << "Branch condition is not integer type." << std::endl;
    return NULL;
  }

  Function *fn = Builder.GetInsertBlock()->getParent();

  BasicBlock *thenBB = BasicBlock::Create(getGlobalContext(), "then", fn);
  BasicBlock *elseBB = BasicBlock::Create(getGlobalContext(), "else");
  BasicBlock *mergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");
  Builder.CreateCondBr(condVal, thenBB, elseBB);

  Builder.SetInsertPoint(thenBB);
  Value *thenVal = Then->Codegen();
  if (thenVal == NULL) return NULL;

  Builder.CreateBr(mergeBB);
  thenBB = Builder.GetInsertBlock();

  fn->getBasicBlockList().push_back(elseBB);
  Builder.SetInsertPoint(elseBB);
  Value *elseVal = Else->Codegen();
  if (elseVal == NULL) return NULL;

  Builder.CreateBr(mergeBB);
  elseBB = Builder.GetInsertBlock();

  fn->getBasicBlockList().push_back(mergeBB);
  Builder.SetInsertPoint(mergeBB);
  PHINode *pn = Builder.CreatePHI(Then->getType(), "iftmp");
  pn->addIncoming(thenVal, thenBB);
  pn->addIncoming(elseVal, elseBB);

  return pn;
}

void Closure::getArgRegs(vector<RegisterFunArg*> &args) {
  vector<RegisterFunArg*> funArgRegs;
  FuncRef->getArgRegs(funArgRegs);
  args.assign(funArgRegs.begin() + ActivationRecord.size(), funArgRegs.end());
}

vector<Expr*> *Closure::getActivationRecord() { return &ActivationRecord; }

// Can only be called after Bind phase.
Value *Closure::GenCallWith(vector<Value*> &argVals) {
  vector<Value*> args;
  vector<string> argNames = FuncRef->getArgNames();
  for (unsigned i=0, e=ActivationRecord.size(); i != e; ++i)
    args.push_back(ActivationRecord[i]->Codegen());
  for (unsigned i=0, e=argVals.size(); i != e; ++i)
    args.push_back(argVals[i]);

  return Builder.CreateCall(
    FuncRef->getFunction(), args.begin(), args.end(), "calltmp");
}

Func *Call::getFunc() {
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    return cl->getFunction();
  } else if (Func *fn = dynamic_cast<Func*>(Callee)) {
    return fn;
  } else {
    std::cerr << "Call to " << CalleeName << " is not function nor closure."
      << std::endl;
    exit(1);
  }
}

void Call::getAllArgs(vector<Expr*> &args) {
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    Func *fn = cl->getFunction();
    vector<Expr*> *rec = cl->getActivationRecord();
    for (unsigned i=0, e=rec->size(); i != e; ++i)
      args.push_back((*rec)[i]);
  }
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    args.push_back(Args[i]);
}

SType *Call::getSType() {
  if (ThisType == NULL)
    ThisType = getFunc()->getFunctionSType()->getReturnType();
  return ThisType;
}

Value *Call::Codegen() {
  vector<Value*> argVals;
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    argVals.push_back(Args[i]->Codegen());

  // XXX: too much dynamic_cast, how about two subclasses of Call?
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    return cl->GenCallWith(argVals);
  } else if (Func *fn = dynamic_cast<Func*>(Callee)) {
    return Builder.CreateCall(
      fn->getFunction(), argVals.begin(), argVals.end(), "calltmp");
  } else {
    std::cerr << "Can only call function or closure." << std::endl;
    exit(1);
  }
}

Value *Closure::Codegen() {
  return FuncRef->getFunction();
}

// Can only be called after Bind phase.
Function *Func::getFunction() {
  if (function != NULL)
    return function;

  FunctionType const *ft = getFunctionSType()->getFunctionType();
  //std::cerr << "FunctionType " << Name << " type: ";
  //ft->dump();
  //std::cerr << std::endl;

  function = Function::Create(ft, Function::ExternalLinkage, Name, TheModule);

  if (function->getName() != Name) {
    std::cout << "Function redeclaration" << std::endl;
    return 0;
  }

  unsigned idx = 0;
  for (Function::arg_iterator ai=function->arg_begin(); idx != Args.size();
      ++ai,++idx)
    ai->setName(Args[idx]);

  return function;
}

void Func::setGenerics(const vector<SType*> &tys) {
  GenericsAreBound = true;
  for (unsigned i=0, e=tys.size(); i != e; ++i)
    GenericPlaceholders[i]->setBinding(tys[i]);
}
void Func::clearGenerics() {
  GenericsAreBound = false;
  for (unsigned i=0, e=GenericPlaceholders.size(); i != e; ++i)
    GenericPlaceholders[i]->setBinding(NULL);
}



Value *Func::Codegen() {
  Function *function = getFunction();

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", function);
  Builder.SetInsertPoint(BB);

  // Allocate registers for arguments, fill in the tree.
  Function::arg_iterator ai = function->arg_begin();
  for (unsigned idx=0, e = Args.size(); idx != e; ++idx, ++ai) {
    IRBuilder<> TmpB(&function->getEntryBlock(),
      function->getEntryBlock().begin());
    AllocaInst *alloca = TmpB.CreateAlloca(
      ArgSTypes[idx]->getType(), 0, Args[idx].c_str());
    Builder.CreateStore(ai, alloca);
    ArgRegs[idx]->setAlloca(alloca);
  }

  Value *ret = Body->Codegen();

  if (ret == NULL) {
    function->eraseFromParent();
    std::cerr << "Empty function: " << Name << std::endl;
    return NULL;
  }

  Builder.CreateRet(ret);
  //function->dump();
  verifyFunction(*function);
  TheFPM->run(*function);

  return function;
}

void Func::MatchGenerics(
    const vector<SType*> &callTypes,
    vector<SType*> &genericBindings) {
  assert(callTypes.size() == Args.size());
  map<SGenericType*, SType*> matchedGenerics;
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    if (SGenericType* gen = dynamic_cast<SGenericType*>(ArgSTypes[i]))
      matchedGenerics[gen] = callTypes[i];
  for (unsigned i=0, e=GenericPlaceholders.size(); i != e; ++i)
    genericBindings[i] = matchedGenerics[GenericPlaceholders[i]];

  // As we are generating type signatures for specialization, all
  // boxed structures are equivalent to us, so we replace them with null.
  for (unsigned i=0, e=genericBindings.size(); i != e; ++i)
    if (SStructType *sty = dynamic_cast<SStructType*>(genericBindings[i]))
      if (!sty->isUnboxed())
        genericBindings[i] = NULL;
}

void File::FindSpecializedFuncs(
    vector<Call*> &calls, map<Func*, set<vector<SType*> >* > &specs) {
  for (unsigned i=0, e=calls.size(); i != e; ++i) {
    Func *fn = calls[i]->getFunc();
    vector<SType*> callTypes;
    {
      vector<Expr*> args;
      calls[i]->getAllArgs(args);
      for (unsigned i=0, e=args.size(); i != e; ++i)
        callTypes.push_back(args[i]->getSType());
    }
    vector<SType*> *genericBindings = new vector<SType*>();
    fn->MatchGenerics(callTypes, *genericBindings);

    bool isAllNull = true;
    for(unsigned i=0, e=genericBindings->size(); i != e; ++i) {
      if ((*genericBindings)[i] != NULL) {
        isAllNull = false;
        break;
      }
    }

    if (specs.count(fn) == 0)
      specs[fn] = new set<vector<SType*> >();
    specs[fn]->insert(*genericBindings);
  }
}

void File::NameSpecializedFuncs(
    vector<Call*> &calls, map<Func*, set<vector<SType*> >* > &specs) {
  //
  //
  //
  //
}

void File::run() {
  InitializeNativeTarget();
  string errStr;
  Module module("my module", getGlobalContext());
  TheModule = &module;
  TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&errStr).create();
  if (!TheExecutionEngine) {
    std::cerr << "ExecutionEngine: " << errStr << std::endl;
    exit(1);
  }
  FunctionPassManager fpm(TheModule);
  TheFPM = &fpm;

  fpm.add(new TargetData(*TheExecutionEngine->getTargetData()));
  fpm.doInitialization();
  /*
  fpm.add(createPromoteMemoryToRegisterPass());
  fpm.add(createInstructionCombiningPass());
  fpm.add(createReassociatePass());
  fpm.add(createGVNPass());
  fpm.add(createCFGSimplificationPass());
  */

  std::cout << "PHASE: LambdaLift" << std::endl;
  LambdaLiftFuncs();

  std::cout << "PHASE: BindTypes" << std::endl;
  NamedTypes["Bool"]  = new SBool();
  NamedTypes["Int8"]  = new Int8();
  NamedTypes["Int16"] = new Int16();
  NamedTypes["Int32"] = new Int32();
  NamedTypes["Int64"] = new Int64();
  for (vector<SType*>::const_iterator i=STypes.begin(); i!=STypes.end(); i++) {
    // TODO check for overloading primitive NamedTypes.count((*i)->getName())
    NamedTypes[(*i)->getName()] = *i;
  }
  for (vector<SType*>::const_iterator i=STypes.begin(); i!=STypes.end(); i++) {
    std::cerr << "Binding " << (*i)->getName() << std::endl;
    vector<string> ResolutionPath;
    (*i)->Bind(ResolutionPath, NamedTypes);
  }
  // Load top-level function names into binding scope.
  map<string, Expr*> NamedExprs;
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    NamedExprs[(*i)->getName()] = *i;

  // PHASE: BindNames.
  std::cout << "PHASE: BindNames" << std::endl;
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    (*i)->Bind(NamedExprs);

  // PHASE: Type inference.
  std::cout << "PHASE: TypeInference" << std::endl;
  multimap<Expr*,Expr*> eqns;
  map<Expr*,SType*> tys;
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++) {
    //std::cerr << "Type inference on: " << (*i)->getName() << std::endl;
    TypeInferer inferer;
    (*i)->TypeInfer(inferer);
    inferer.TypeUnification();
    inferer.TypePopulation();
  }

  // PHASE: Specialization Codegen
  {
    std::cout << "PHASE: Specialization Codegen" << std::endl;
    vector<Call*> calls;
    map<Func*, set< vector<SType*> >* > specializedFuncs;
    for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
      (*i)->FindCalls(calls);
    FindSpecializedFuncs(calls, specializedFuncs);
    NameSpecializedFuncs(calls, specializedFuncs);
    map<Func*, set<vector<SType*> >* >::const_iterator it;
    for (it=specializedFuncs.begin(); it != specializedFuncs.end(); ++it) {
      Func *fn = it->first;
      set<vector<SType*> >* specs = it->second;
      set<vector<SType*> >::iterator i;
      for (i=specs->begin(); i != specs->end(); ++i) {
        fn->setGenerics(*i);
        fn->Codegen();
        fn->clearGenerics();
      }
    }
  }

  // PHASE: Code generation
  std::cout << "PHASE: Codegen" << std::endl;
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++) {
    // TODO: pass in the LLVM state as an argument.
    (*i)->Codegen();
  }

  std::cout << "LLVM IR dump:" << std::endl;

  TheModule->dump();

  // Execute the function `main'.
  Function *f = module.getFunction("main");
  if (f == NULL) {
    std::cout << "main is not defined!" << std::endl;
    exit(1);
  }
  void *fptr = TheExecutionEngine->getPointerToFunction(f);
  int32_t (*fp)() = (int32_t (*)())(intptr_t)fptr;
  std::cout << "Result: " << fp() << std::endl;
}

}; };
