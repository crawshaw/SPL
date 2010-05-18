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

Type const *Expr::getType() {
  return Type::getInt32Ty(getGlobalContext());
}
Type const *Closure::getType() {
  std::cout << "closure tag 1" << std::endl;
  const FunctionType *ft = FuncRef->getFunction()->getFunctionType();
  std::cout << "closure tag 2" << std::endl;
  return PointerType::getUnqual(ft);
}

Type const *Number::getType() { return Type::getInt32Ty(getGlobalContext()); }
Type const *BinaryOp::getType() { return RHS->getType(); }
Type const *Eq::getType() { return Type::getInt1Ty(getGlobalContext()); }
Type const *Bind::getType() { return Body->getType(); }
Type const *If::getType() { return Then->getType(); }


Value *Number::Codegen(map<string, Expr*> &NamedExprs) {
  return ConstantInt::get(getGlobalContext(), APInt(32, Val, true));
}

Value *Variable::Codegen(map<string, Expr*> &NamedExprs) {
  Value *V = NamedExprs[Name]->Codegen(NamedExprs);
  if (V == 0) {
    std::cerr << "Unknown variable: `" << Name << "'" << std::endl;
    return NULL;
  }
  return Builder.CreateLoad(V, Name.c_str());
}

Value *Not::Codegen(map<string, Expr*> &NamedExprs) {
  return NULL;
}

Value *Add::Codegen(map<string, Expr*> &NamedExprs) {
  return Builder.CreateAdd(LHS->Codegen(NamedExprs), RHS->Codegen(NamedExprs), "addtmp");
}

Value *Subtract::Codegen(map<string, Expr*> &NamedExprs) {
  return Builder.CreateSub(LHS->Codegen(NamedExprs), RHS->Codegen(NamedExprs), "subtmp");
}

Value *Multiply::Codegen(map<string, Expr*> &NamedExprs) {
  return Builder.CreateMul(LHS->Codegen(NamedExprs), RHS->Codegen(NamedExprs), "multmp");
}

Value *Eq::Codegen(map<string, Expr*> &NamedExprs) {
  return Builder.CreateICmpEQ(LHS->Codegen(NamedExprs), RHS->Codegen(NamedExprs), "eqtmp");
}

Value *Seq::Codegen(map<string, Expr*> &NamedExprs) {
  return NULL;
}

Value *Bind::Codegen(map<string, Expr*> &NamedExprs) {
  Value *InitVal = Init->Codegen(NamedExprs);
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());
  AllocaInst *Alloca = TmpB.CreateAlloca(Init->getType(), 0, Name.c_str());

  Builder.CreateStore(InitVal, Alloca);

  Expr *OldExpr = NamedExprs[Name];
  Expr *reg = new Register(Alloca);
  NamedExprs[Name] = reg;

  Value *BodyValue = Body->Codegen(NamedExprs);
  if (BodyValue == 0) return 0;

  NamedExprs[Name] = OldExpr;
  delete reg;

  return BodyValue;
}

Value *If::Codegen(map<string, Expr*> &NamedExprs) {
  Value *condVal = Cond->Codegen(NamedExprs);
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
  Value *thenVal = Then->Codegen(NamedExprs);
  if (thenVal == NULL) return NULL;

  Builder.CreateBr(mergeBB);
  thenBB = Builder.GetInsertBlock();

  fn->getBasicBlockList().push_back(elseBB);
  Builder.SetInsertPoint(elseBB);
  Value *elseVal = Else->Codegen(NamedExprs);
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

Value *Closure::GenCallWith(
    vector<Value*> &argVals, map<string,Expr*> &NamedExprs) {
  vector<Value*> args;
  vector<string> argNames = FuncRef->getArgNames();
  for (unsigned i=0, e=argNames.size(); i != e; ++i)
    args.push_back(
      NamedExprs[ActivationRecord[argNames[i]]]->Codegen(NamedExprs));
  for (unsigned i=0, e=argVals.size(); i != e; ++i)
    args.push_back(argVals[i]);

  return Builder.CreateCall(
    FuncRef->getFunction(), args.begin(), args.end(), "calltmp");
}

Value *Call::Codegen(map<string, Expr*> &NamedExprs) {
  vector<Value*> argVals;
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    argVals.push_back(Args[i]->Codegen(NamedExprs));

  if (Closure *cl = dynamic_cast<Closure*>(NamedExprs[CalleeName])) {
    return cl->GenCallWith(argVals, NamedExprs);
  } else if (Func *fn = dynamic_cast<Func*>(NamedExprs[CalleeName])) {
    return Builder.CreateCall(
      fn->getFunction(), argVals.begin(), argVals.end(), "calltmp");
  } else {
    std::cerr << "Can only call function or closure." << std::endl;
    exit(1);
  }
}

Value *Closure::Codegen(map<string, Expr*> &NamedExprs) {
  return FuncRef->getFunction();
}

Value *Register::Codegen(map<string, Expr*> &NamedExprs) {
  return Alloca;
}

vector<AllocaInst*> *Func::createArgAllocas() {
  vector<AllocaInst*> *argAllocas = new vector<AllocaInst*>();

  Function::arg_iterator ai = function->arg_begin();
  for (unsigned idx=0, e = Args.size(); idx != e; ++idx, ++ai) {
    IRBuilder<> TmpB(&function->getEntryBlock(),
      function->getEntryBlock().begin());
    AllocaInst *alloca = TmpB.CreateAlloca(
      Type::getInt32Ty(getGlobalContext()), 0, Args[idx].c_str());
    Builder.CreateStore(ai, alloca);
    argAllocas->push_back(alloca);
  }

  return argAllocas;
}

Function *Func::getFunction() {
  if (function)
    return function;

  vector<const Type*> ArgTypes(Args.size(),
    Type::getInt32Ty(getGlobalContext()));
  FunctionType *ft = FunctionType::get(Type::getInt32Ty(
    getGlobalContext()), ArgTypes, false);
  function = Function::Create(ft, Function::ExternalLinkage, Name, TheModule);

  if (function->getName() != Name) {
    // Terrible conflict
    std::cout << "Function redeclaration" << std::endl;
    return 0;
  }

  unsigned idx = 0;
  for (Function::arg_iterator ai=function->arg_begin(); idx != Args.size();
      ++ai,++idx)
    ai->setName(Args[idx]);

  return function;
}

Value *Func::Codegen(map<string, Expr*> &NamedExprs) {
  Function *function = getFunction();

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", function);
  Builder.SetInsertPoint(BB);

  vector<Expr*> oldBindings;
  vector<AllocaInst*> &argAllocas = *createArgAllocas();
  assert(Args.size() == argAllocas.size());

  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    oldBindings.push_back(
      NamedExprs.count(Args[i]) == 0 ? NULL : NamedExprs[Args[i]]);
    NamedExprs[Args[i]] = new Register(argAllocas[i]);
  }

  Value *ret = Body->Codegen(NamedExprs);

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    NamedExprs[Args[i]] = oldBindings[i];

  if (ret == NULL) {
    function->eraseFromParent();
    return NULL;
  }

  Builder.CreateRet(ret);
  verifyFunction(*function);
  // TODO: TheFPM->run(*function);

  return function;
}

void File::run() {
  std::cout << "tag 1" << std::endl;
  string errStr;
  Module module("my module", getGlobalContext());
  TheModule = &module;
  TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&errStr).create();
  FunctionPassManager fpm(TheModule);
  std::cout << "tag 2" << std::endl;
  TheFPM = &fpm;

  fpm.doInitialization();
  /*
  fpm.add(new TargetData(*TheExecutionEngine->getTargetData()));
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

  map<string, Expr*> NamedExprs;

  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    NamedExprs[(*i)->getName()] = *i;

  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    (*i)->Codegen(NamedExprs);

  std::cout << "tag 6" << std::endl;

  TheModule->dump();
  std::cout << "tag 7" << std::endl;
}

}; };
