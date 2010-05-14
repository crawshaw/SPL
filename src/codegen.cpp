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

static std::map<std::string, AllocaInst*> NamedValues;

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
  llvm::Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());
  AllocaInst *Alloca = TmpB.CreateAlloca(
    Type::getDoubleTy(getGlobalContext()), 0, Name.c_str());

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

Value *Func::Codegen() {
  return NULL;
}

Value *File::Codegen() {
  for (std::vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++){
    (*i)->Codegen();
  }

  return NULL;
}

Value *Closure::Codegen() {
  return NULL;
}

}; };
