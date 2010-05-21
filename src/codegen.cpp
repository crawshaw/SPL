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
static map<string,SType*> NamedTypes;


/////////////////////////////////////////////////////////////////////


Type const *Expr::getType() { return ThisType->getType(); }

Type const* Int8 ::getType() { return Type::getInt8Ty( getGlobalContext()); }
Type const* Int16::getType() { return Type::getInt16Ty(getGlobalContext()); }
Type const* Int32::getType() { return Type::getInt32Ty(getGlobalContext()); }
Type const* Int64::getType() { return Type::getInt64Ty(getGlobalContext()); }
Type const* SStructType::getType() { return ThisType; }
void SStructType::Bind(map<string, SType*> &NamedTypes) {
  for (unsigned i=0, e=ElementSTypeNames.size(); i != e; ++i)
    ElementSTypes.push_back(NamedTypes[ElementSTypeNames[i]]);
  vector<const Type *> tys;
  for (unsigned i=0, e=ElementSTypes.size(); i != e; ++i)
    tys.push_back(ElementSTypes[i]->getType());
  ThisType = StructType::get(getGlobalContext(), tys);
}
Type const *SStructType::getPassType() {
  return PointerType::getUnqual(getType());
}
FunctionType const *SFunctionType::getFunctionType() {
  vector<const Type*> ArgTypes;
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    ArgTypes.push_back(Args[i]->getType());
  return FunctionType::get(Ret->getPassType(), ArgTypes, false);
}
Type const *SFunctionType::getType() { return getFunctionType(); }
Type const *SPtr::getType() { return PointerType::getUnqual(Ref->getType()); }
Type const *SBool::getType() { return Type::getInt1Ty(getGlobalContext()); }


/////////////////////////////////////////////////////////////////////


void Number::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  tys[this] = new Int32();
}
void Variable::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this, Binding));
}
void UnaryOp::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this, SubExpr));
  SubExpr->TypeInfer(eqns, tys);
}
void BinaryOp::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this, LHS));
  eqns.insert(pair<Expr*,Expr*>(this, RHS));
  LHS->TypeInfer(eqns, tys);
  RHS->TypeInfer(eqns, tys);
}
void Eq::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  tys[this] = new SBool();
  LHS->TypeInfer(eqns, tys);
  RHS->TypeInfer(eqns, tys);
}
void Seq::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,RHS));
  LHS->TypeInfer(eqns, tys);
  RHS->TypeInfer(eqns, tys);
}
void Member::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  // TODO
}
void Binding::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  Init->TypeInfer(eqns, tys);
  Body->TypeInfer(eqns, tys);
}
void If::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  Cond->TypeInfer(eqns, tys);
  Then->TypeInfer(eqns, tys);
  Else->TypeInfer(eqns, tys);
  tys[Cond] = new SBool();
  eqns.insert(pair<Expr*,Expr*>(this,Then));
  eqns.insert(pair<Expr*,Expr*>(this,Else));
}
void Call::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,Callee));

  // Match each Arg to the associate Func register.
  vector<RegisterFunArg*> argRegs;
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    cl->getArgRegs(argRegs);
  } else if (Func *fn = dynamic_cast<Func*>(Callee)) {
    fn->getArgRegs(argRegs);
  } else {
    std::cerr << "Call to " << CalleeName << " is not function nor closure."
      << std::endl;
    exit(1);
  }

  assert(argRegs.size() == Args.size());
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    eqns.insert(pair<Expr*,Expr*>(Args[i], argRegs[i]));
    Args[i]->TypeInfer(eqns, tys);
  }
}
void Register::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,Source));
}
void Func::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,Body));
  Body->TypeInfer(eqns, tys);
}
void Closure::TypeInfer(multimap<Expr*,Expr*> &eqns, map<Expr*,SType*> &tys) {
  eqns.insert(pair<Expr*,Expr*>(this,FuncRef));
  vector<RegisterFunArg*> argRegs;
  FuncRef->getArgRegs(argRegs);

  unsigned idx = 0;
  map<string,Expr*>::const_iterator it;
  for (it=ActivationRecord.begin(); it!=ActivationRecord.end(); ++it, ++idx)
    eqns.insert(pair<Expr*,Expr*>((*it).second, argRegs[idx]));
}
void Constructor::TypeInfer(multimap<Expr*,Expr*>&eqns, map<Expr*,SType*> &tys){
  SType *ty = NamedTypes[STypeName];
  if (ty == NULL) {
    std::cerr << "Unknown type: " << STypeName << std::endl;
    exit(1);
  }
  tys[this] = ty;
}


/////////////////////////////////////////////////////////////////////


void Number::Bind(map<string, Expr*> &) {
  ThisType = new Int32();
}

void Variable::Bind(map<string, Expr*> &NamedExprs) {
  Binding = NamedExprs[Name];
  if (Binding) {
    ThisType = Binding->getSType();
  } else {
    std::cerr << "Failed to bind " << Name << std::endl;
  }
}

void UnaryOp::Bind(map<string, Expr*> &NamedExprs) {
  SubExpr->Bind(NamedExprs);
  ThisType = SubExpr->getSType();
}

void BinaryOp::Bind(map<string, Expr*> &NamedExprs) {
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
  ThisType = RHS->getSType();
}

void Eq::Bind(map<string, Expr*> &NamedExprs) {
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
  ThisType = new SBool();
}

void Member::Bind(map<string, Expr*> &NamedExprs) {
  // TODO
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
  std::cerr << "Member::Bind not yet implemented!" << std::endl;
  // TODO setSType(RHS->getSType());
}

void Constructor::Bind(map<string, Expr*> &NamedExprs) {
  if (SStructType *st = dynamic_cast<SStructType*>(NamedTypes[STypeName])) {
    ThisType = st;
  } else {
    std::cerr << "No type matching constructor: " << STypeName << std::endl;
  }

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->Bind(NamedExprs);
}

void Binding::Bind(map<string, Expr*> &NamedExprs) {
  Init->Bind(NamedExprs);

  Expr *OldExpr = NamedExprs[Name];
  Expr *reg = new Register(Name, Init, Init->getSType());
  reg->Bind(NamedExprs);
  NamedExprs[Name] = reg;

  Body->Bind(NamedExprs);

  NamedExprs[Name] = OldExpr;

  ThisType = Body->getSType();
}

void If::Bind(map<string, Expr*> &NamedExprs) {
  Cond->Bind(NamedExprs);
  Then->Bind(NamedExprs);
  Else->Bind(NamedExprs);
  ThisType = Then->getSType();
}

void Closure::Bind(map<string,Expr*> &NamedExprs) {
  map<string,string>::const_iterator it;
  for (it=ActivationRecordNames.begin(); it!=ActivationRecordNames.end(); ++it)
    ActivationRecord[(*it).first] =
      NamedExprs[ActivationRecordNames[(*it).second]];
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
  // Resolve return type. TODO remove Int32 hack, add type resolution phase.
  std::string int32("Int32");
  RetSType = NamedTypes[RetSTypeName == NULL ? int32 : *RetSTypeName];
  //std::cerr << "Function " << Name << " return type: ";
  //RetSType->getType()->dump();
  //std::cerr << std::endl;

  // Resolve argument types.
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    // TODO: remove, add type resolution phase.
    SType *ty = NamedTypes[ArgSTypeNames[i]==NULL ? int32 : *ArgSTypeNames[i]];
    ArgSTypes.push_back(ty);
  }

  ThisType = new SFunctionType(Name, ArgSTypes, RetSType);

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
  return NULL;
}

Value *Constructor::Codegen() {
  // TODO: heap allocation.
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());
  AllocaInst *a = TmpB.CreateAlloca(ThisType->getType(), 0, STypeName.c_str());

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

// Can only be called after Bind phase.
Value *Closure::GenCallWith(vector<Value*> &argVals) {
  vector<Value*> args;
  vector<string> argNames = FuncRef->getArgNames();
  for (unsigned i=0, e=argNames.size(); i != e; ++i)
    args.push_back(ActivationRecord[argNames[i]]->Codegen());
  for (unsigned i=0, e=argVals.size(); i != e; ++i)
    args.push_back(argVals[i]);

  return Builder.CreateCall(
    FuncRef->getFunction(), args.begin(), args.end(), "calltmp");
}

SType *Call::getSType() {
  if (ThisType == NULL) {
    if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
      ThisType = cl->getFunction()->getFunctionSType()->getReturnType();
    } else if (Func *fn = dynamic_cast<Func*>(Callee)) {
      ThisType = fn->getFunctionSType()->getReturnType();
    } else {
      std::cerr << "Call to " << CalleeName << " is not function nor closure."
        << std::endl;
      exit(1);
    }
  }
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

  FunctionType const *ft = ThisType->getFunctionType();
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


Value *Func::Codegen() {
  Function *function = getFunction();
  std::cerr << "Generating code for " << Name <<std::endl;

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
  // TODO: TheFPM->run(*function);

  return function;
}

void File::run() {
  InitializeNativeTarget();
  std::cout << "tag 1" << std::endl;
  string errStr;
  Module module("my module", getGlobalContext());
  TheModule = &module;
  TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&errStr).create();
  if (!TheExecutionEngine) {
    std::cerr << "ExecutionEngine: " << errStr << std::endl;
    exit(1);
  }
  FunctionPassManager fpm(TheModule);
  std::cout << "tag 2" << std::endl;
  TheFPM = &fpm;

  fpm.add(new TargetData(*TheExecutionEngine->getTargetData()));
  fpm.doInitialization();
  /*
  std::cout << "tag 3" << std::endl;
  fpm.add(createPromoteMemoryToRegisterPass());
  fpm.add(createInstructionCombiningPass());
  fpm.add(createReassociatePass());
  fpm.add(createGVNPass());
  fpm.add(createCFGSimplificationPass());
  */
  std::cout << "tag 4" << std::endl;

  LambdaLiftFuncs();
  std::cout << "tag 5" << std::endl;

  // Load built-in types into type scope.
  // NamedTypes["Bool"] = TODO
  NamedTypes["Int8"]  = new Int8();
  NamedTypes["Int16"] = new Int16();
  NamedTypes["Int32"] = new Int32();
  NamedTypes["Int64"] = new Int64();

  // Bind and load top-level type definitions into the type scope.
  for (vector<SType*>::const_iterator i=STypes.begin(); i!=STypes.end(); i++) {
    (*i)->Bind(NamedTypes);
    // TODO check for overloading primitive NamedTypes.count((*i)->getName())
    NamedTypes[(*i)->getName()] = *i;
  }

  // Load top-level function names into binding scope.
  map<string, Expr*> NamedExprs;
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    NamedExprs[(*i)->getName()] = *i;

  std::cout << "tag 5b" << std::endl;

  // Bind all functions.
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    (*i)->Bind(NamedExprs);

  std::cout << "tag 5c" << std::endl;

  // Generate LLVM IR for each top-level function.
  // TODO: pass in the LLVM state as an argument.
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    (*i)->Codegen();

  std::cout << "tag 6" << std::endl;

  TheModule->dump();
  std::cout << "tag 7" << std::endl;

  // Execute the function `main'.
  Function *f = module.getFunction("main");
  if (f == NULL) {
    std::cout << "main is not defined!" << std::endl;
  }
  void *fptr = TheExecutionEngine->getPointerToFunction(f);
  int32_t (*fp)() = (int32_t (*)())(intptr_t)fptr;
  std::cout << "Result: " << fp() << std::endl;

  std::cout << "tag 8" << std::endl;
}

}; };
