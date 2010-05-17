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

using namespace llvm;

static IRBuilder<> Builder(getGlobalContext());

namespace SPL { namespace AST {

// TODO: not globals
static std::map<std::string, AllocaInst*> NamedValues;
static Module *TheModule;
static FunctionPassManager *TheFPM;
static ExecutionEngine *TheExecutionEngine;

Value *Number::Codegen() {
  return ConstantInt::get(getGlobalContext(), APInt(32, Val, true));
}

Value *Variable::Codegen() {
  Value *V = NamedValues[Name];
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

Value *Bind::Codegen() {
  Value *InitVal = Body->Codegen();
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());
  AllocaInst *Alloca = TmpB.CreateAlloca(
    Type::getInt32Ty(getGlobalContext()), 0, Name.c_str());

  Builder.CreateStore(InitVal, Alloca);

  AllocaInst *OldBinding = NamedValues[Name];
  NamedValues[Name] = Alloca;

  Value *BodyValue = Body->Codegen();
  if (BodyValue == 0) return 0;

  NamedValues[Name] = OldBinding;

  return BodyValue;
}

Value *If::Codegen() {
  return NULL;
}

Value *Call::Codegen() {
  return NULL;
}

void Func::createArgAllocas() {
  Function::arg_iterator ai = function->arg_begin();
  for (unsigned idx=0, e = Args.size(); idx != e; ++idx, ++ai) {
    IRBuilder<> TmpB(&function->getEntryBlock(),
      function->getEntryBlock().begin());
    AllocaInst *alloca = TmpB.CreateAlloca(
      Type::getInt32Ty(getGlobalContext()), 0, Args[idx].c_str());
    Builder.CreateStore(ai, alloca);
    // TODO: NamedValues[Args[idx]] = Alloca;
  }
}

Value *Func::Codegen() {
  std::vector<const Type*> ArgTypes(Args.size(),
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

  // Create a new basic block to start insertion into.
  BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", function);
  Builder.SetInsertPoint(BB);

  createArgAllocas();

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

Value *File::Codegen() {
  for (std::vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++){
    (*i)->Codegen();
  }

  return NULL;
}

void File::run() {
  std::cout << "tag 1" << std::endl;
  std::string errStr;
  Module module("my module", getGlobalContext());
  TheModule = &module;
  TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&errStr).create();
  FunctionPassManager fpm(TheModule);
  std::cout << "tag 2" << std::endl;
  TheFPM = &fpm;

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
  Codegen();
  std::cout << "tag 6" << std::endl;

  TheModule->dump();
  std::cout << "tag 7" << std::endl;
}

Value *Closure::Codegen() {
  return NULL;
}

}; };
