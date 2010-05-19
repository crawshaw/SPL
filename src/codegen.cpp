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
Type const *Binding::getType() { return Body->getType(); }
Type const *If::getType() { return Then->getType(); }
Type const *Member::getType() { return NULL; /* TODO */ }


Value *Number::Codegen() {
  return ConstantInt::get(getGlobalContext(), APInt(32, Val, true));
}

void Variable::Bind(map<string, Expr*> &NamedExprs) {
  Binding = NamedExprs[Name];
}

Value *Variable::Codegen() {
  Value *V = Binding->Codegen();
  if (V == 0) {
    std::cerr << "Unknown variable: `" << Name << "'" << std::endl;
    return NULL;
  }
  return Builder.CreateLoad(V, Name.c_str());
}

void UnaryOp::Bind(map<string, Expr*> &NamedExprs) {
  SubExpr->Bind(NamedExprs);
}

Value *Not::Codegen() {
  return NULL;
}

void BinaryOp::Bind(map<string, Expr*> &NamedExprs) {
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
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

void Member::Bind(map<string, Expr*> &NamedExprs) {
  // TODO
  LHS->Bind(NamedExprs);
  RHS->Bind(NamedExprs);
}

Value *Member::Codegen() {
  return NULL;
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

void Binding::Bind(map<string, Expr*> &NamedExprs) {
  Init->Bind(NamedExprs);

  Expr *OldExpr = NamedExprs[Name];
  Expr *reg = new Register(Name, Init);
  NamedExprs[Name] = reg;

  Body->Bind(NamedExprs);

  NamedExprs[Name] = OldExpr;
}

Value *Binding::Codegen() {
  return Body->Codegen();
}

void If::Bind(map<string, Expr*> &NamedExprs) {
  Cond->Bind(NamedExprs);
  Then->Bind(NamedExprs);
  Else->Bind(NamedExprs);
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

void Closure::Bind(map<string,Expr*> &NamedExprs) {
  vector<string> args = FuncRef->getArgNames();
  for (unsigned i=0, e=args.size(); i != e; ++i)
    ActivationRecord[args[i]] = NamedExprs[ActivationRecordNames[args[i]]];
}

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

void Call::Bind(map<string, Expr*> &NamedExprs) {
  Callee = NamedExprs[CalleeName];
}

Value *Call::Codegen() {
  vector<Value*> argVals;
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    argVals.push_back(Args[i]->Codegen());

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

void Func::Bind(map<string, Expr*> &NamedExprs) {
  vector<Expr*> oldBindings;

  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    RegisterFunArg *reg = new RegisterFunArg();
    ArgRegs.push_back(reg);
    oldBindings.push_back(
      NamedExprs.count(Args[i]) == 0 ? NULL : NamedExprs[Args[i]]);
    NamedExprs[Args[i]] = reg;
  }

  Body->Bind(NamedExprs);

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    NamedExprs[Args[i]] = oldBindings[i];
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
      Type::getInt32Ty(getGlobalContext()), 0, Args[idx].c_str());
    Builder.CreateStore(ai, alloca);
    ArgRegs[idx]->setAlloca(alloca);
  }

  Value *ret = Body->Codegen();

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
    (*i)->Bind(NamedExprs);

  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    (*i)->Codegen();

  std::cout << "tag 6" << std::endl;

  TheModule->dump();
  std::cout << "tag 7" << std::endl;
}

}; };
