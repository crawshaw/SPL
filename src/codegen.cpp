#include "ast.h"

#include "llvm/LLVMContext.h"
#include "llvm/PassManager.h"
#include "llvm/Analysis/Verifier.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Support/StandardPasses.h"

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

// TODO: pass to Bind() along with NamedExprs.
map<string,SType*> NamedTypes;

map<string,SType*> SType::BuiltinsMap;
const map<string,SType*> &SType::Builtins() {
  if (BuiltinsMap.size() == 0) {
    BuiltinsMap["Bool"]  = new SBool();
    BuiltinsMap["Int8"]  = new Int8();
    BuiltinsMap["Int16"] = new Int16();
    BuiltinsMap["Int32"] = new Int32();
    BuiltinsMap["Int64"] = new Int64();
    BuiltinsMap["String"] = new SString();
  }
  return BuiltinsMap;
}


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
SArray *ArrayAccess::getSourceSType() {
  SArray *sty = dynamic_cast<SArray*>(LHS->getSType());
  if (sty == NULL) {
    std::cerr << "Attempting array access on non-array type:";
    LHS->getSType()->dump();
    std::cerr << std::endl;
    exit(1);
  }
  return sty;
}

/////////////////////////////////////////////////////////////////////


void Number::Bind(map<string, Expr*> &) { }
void StringLiteral::Bind(map<string, Expr*> &) { }

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

void Member::Bind(map<string, Expr*> &NamedExprs) {
  Source->Bind(NamedExprs);
}

void Array::Bind(map<string, Expr*> &NamedExprs) {
  SizeExpr->Bind(NamedExprs);
  DefaultValue->Bind(NamedExprs);
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

void While::Bind(map<string, Expr*> &NamedExprs) {
  Cond->Bind(NamedExprs);
  Body->Bind(NamedExprs);
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

  vector<SType*> oldTypeBindings;
  for (unsigned i=0, e=Generics.size(); i != e; ++i) {
    SGenericType *ty = new SGenericType(Generics[i]);
    GenericPlaceholders.push_back(ty);
    oldTypeBindings.push_back(NamedTypes[Generics[i]]);
    NamedTypes[Generics[i]] = ty;
  }

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

  if (Body)
    Body->Bind(NamedExprs);

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    NamedExprs[Args[i]] = oldBindings[i];

  for (unsigned i=0, e=Generics.size(); i != e; ++i)
    NamedTypes[Generics[i]] = oldTypeBindings[i];
}


/////////////////////////////////////////////////////////////////////


void Number::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) { }
void StringLiteral::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) { }
void Variable::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) { }
void UnaryOp::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  SubExpr->FindCalls(calls);
}
void BinaryOp::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  LHS->FindCalls(calls);
  RHS->FindCalls(calls);
}
void Member::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  Source->FindCalls(calls);
}
void Binding::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  Init->FindCalls(calls);
  Body->FindCalls(calls);
}
void If::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  Cond->FindCalls(calls);
  Then->FindCalls(calls);
  Else->FindCalls(calls);
}
void While::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  Cond->FindCalls(calls);
  Body->FindCalls(calls);
}
void Call::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->FindCalls(calls);

  vector<SType*> genericBindings;
  Func *fn = getFunc();
  getGenerics(genericBindings);
  pair<Func*, vector<SType*> > res(fn, genericBindings);

  if (Util::contains(calls, res))
    return;

  calls.push_back(res);
  vector<SType*> oldGenericBindings;
  fn->getGenerics(oldGenericBindings);
  fn->setGenerics(genericBindings);

  // Update the CalleeName and return type to match specialization.
  fn->getFullName(CalleeName);
  SType *ret = fn->getFunctionSType()->getReturnType();
  if (SGenericType *genRet = dynamic_cast<SGenericType*>(ret)) {
    if (SType *boundRet = genRet->getBinding()) {
      setSType(boundRet);
    }
  }
  fn->FindCalls(calls);
  fn->setGenerics(oldGenericBindings);
}
void Func::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  if (Body)
    Body->FindCalls(calls);
}
void Closure::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  // TODO: what about the ActivationRecord?
}
void Array::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  SizeExpr->FindCalls(calls);
  DefaultValue->FindCalls(calls);
}
void Constructor::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    Args[i]->FindCalls(calls);
}
void Register::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {}
void RegisterFunArg::FindCalls(vector<pair<Func*,vector<SType*> > > &calls) {}


/////////////////////////////////////////////////////////////////////

Value *Expr::LValuegen() {
  std::cerr << "Called LValuegen on Expr that is not an LValue." << std::endl;
  exit(1);
}

Value *Variable::LValuegen() {
  Value *V = Binding->Codegen();
  if (V == 0) {
    std::cerr << "Unknown variable: `" << Name << "'" << std::endl;
    exit(1);
  }
  return V;
}

Value *ArrayAccess::LValuegen() {
  const Type *Contained = getSourceSType()->getContained()->getType();
  Value *src = LHS->Codegen();
  Value *array = Builder.CreateLoad(Builder.CreateStructGEP(src, 1));
  Value *castVal =
    Builder.CreateBitCast(array, PointerType::getUnqual(Contained));
  Value *val = Builder.CreateGEP(castVal, RHS->Codegen());
  return val;
}

Value *Member::LValuegen() {
  unsigned fieldIndex = getSourceSType()->getIndex(FieldName);
  Value *src = Source->Codegen();
  return Builder.CreateStructGEP(src, fieldIndex);
}

/////////////////////////////////////////////////////////////////////


Value *Number::Codegen() {
  return ConstantInt::get(getGlobalContext(), APInt(32, Val, true));
}

Value *Variable::Codegen() {
  return Builder.CreateLoad(LValuegen(), Name.c_str());
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

Value *StringLiteral::Codegen() {

  // Heap allocate the structure containing the size.
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());

  // TODO: are we repeating ourselves here when the same string is repeated?
  Value *glb = Builder.CreateGlobalString(Str.c_str());
  glb = TmpB.CreateBitCast(glb, PointerType::getUnqual(
        ArrayType::get(Type::getInt8Ty(getGlobalContext()), 0)));

  Function *mallocFunc = TheModule->getFunction("GC_malloc");
  Type const *ty = ThisType->getPassType();
  Value *mallocArg = ConstantExpr::getSizeOf(ty);

  Value *val = TmpB.CreateCall(mallocFunc, mallocArg);
  Value *castVal = TmpB.CreateBitCast(val, PointerType::getUnqual(ty));

  Value *sizeGep = TmpB.CreateStructGEP(castVal, 0);
  ConstantInt *size =
    ConstantInt::getSigned(Type::getInt32Ty(getGlobalContext()), Str.length());
  Builder.CreateStore(size, sizeGep);

  Value *ptrGep = TmpB.CreateStructGEP(castVal, 1);
  Builder.CreateStore(glb, ptrGep);

  return castVal;
}

Value *JoinString::Codegen() {
  // TODO: malloc a new string
  return NULL;
}

Value *Seq::Codegen() {
  LHS->Codegen();
  return RHS->Codegen();
}

Value *Assign::Codegen() {
  Value *Val = RHS->Codegen();
  Builder.CreateStore(Val, LHS->LValuegen());
  return Val;
}

Value *ArrayAccess::Codegen() {
  return Builder.CreateLoad(LValuegen());
}

Value *Member::Codegen() {
  return Builder.CreateLoad(LValuegen());
}

Value *Array::Codegen() {
  Value *arraySize = SizeExpr->Codegen();

  // Heap allocation.
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    TheFunction->getEntryBlock().begin());

  Type const *ty = ThisType->getPassType();
  Function *mallocFunc = TheModule->getFunction("GC_malloc");
  Value *mallocArg = ConstantExpr::getSizeOf(ty);

  Value *val = TmpB.CreateCall(mallocFunc, mallocArg);
  Value *castVal = TmpB.CreateBitCast(val, PointerType::getUnqual(ty));
  std::cout << "tag 3" << std::endl;

  Value *sizePtr = TmpB.CreateStructGEP(castVal, 0);
  Builder.CreateStore(arraySize, sizePtr);
  arraySize = TmpB.CreateIntCast(arraySize,
      Type::getInt64Ty(getGlobalContext()), true);

  const Type *contained = Contained->getType();
  Value *sizeofC = ConstantExpr::getSizeOf(contained);
  Value *arrayBytes = TmpB.CreateMul(arraySize, sizeofC);
  Value *arrVal = TmpB.CreateCall(mallocFunc, arrayBytes);
  arrVal = TmpB.CreateBitCast(arrVal, PointerType::getUnqual(
        ArrayType::get(contained, 0)));

  std::cout << "tag 4" << std::endl;
  // Set initial values.
  {
    // TODO:  This is a repeat of the code in While::Codegen(). Instead,
    //        let's just create an AST fragment for this that uses While,
    //        and Codegen() it here.
    if (DefaultValue == 0)
      std::cout << "unexpected null default value." << std::endl;
    Value *defaultVal = DefaultValue->Codegen();
    Value *StartVal = ConstantInt::get(getGlobalContext(), APInt(64, 0, true));

    // Make the new basic block for the loop header, inserting after current block.
    Function *TheFunction = Builder.GetInsertBlock()->getParent();
    BasicBlock *PreheaderBB = Builder.GetInsertBlock();
    BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
    // Insert an explicit fall through from the current block to the LoopBB.
    Builder.CreateBr(LoopBB);
    // Start insertion in LoopBB.
    Builder.SetInsertPoint(LoopBB);
    
    // Start the PHI node with an entry for Start.
    PHINode *Variable = Builder.CreatePHI(Type::getInt64Ty(getGlobalContext()));
    Variable->addIncoming(StartVal, PreheaderBB);

    // Emit the body of the loop.
    Value *arrayPos = Builder.CreateGEP(arrVal, Variable);
    arrayPos = Builder.CreateBitCast(arrayPos, PointerType::getUnqual(contained));
    Builder.CreateStore(defaultVal, arrayPos);
    
    // Emit the step value.
    Value *StepVal = ConstantInt::get(getGlobalContext(), APInt(64, 1, true));
    Value *NextVar = Builder.CreateAdd(Variable, StepVal, "nextvar");

    Value *EndCond = Builder.CreateICmpEQ(NextVar, arraySize, "loopcond");

    // Create the "after loop" block and insert it.
    BasicBlock *LoopEndBB = Builder.GetInsertBlock();
    BasicBlock *AfterBB = BasicBlock::Create(
      getGlobalContext(), "afterloop", TheFunction);
    
    // Insert the conditional branch into the end of LoopEndBB.
    Builder.CreateCondBr(EndCond, LoopBB, AfterBB);
    
    // Any new code will be inserted in AfterBB.
    Builder.SetInsertPoint(AfterBB);
    
    // Add a new entry to the PHI node for the backedge.
    Variable->addIncoming(NextVar, LoopEndBB);
  }
  
  Value *arrPtr = Builder.CreateStructGEP(castVal, 1);
  Builder.CreateStore(arrVal, arrPtr);

  return castVal;
}

Value *Constructor::Codegen() {
  // Heap allocation.
  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  Type const *ty = ThisType->getPassType();

  Function *mallocFunc = TheModule->getFunction("GC_malloc");
  Value *mallocArg = ConstantExpr::getSizeOf(ty);

  Value *val = Builder.CreateCall(mallocFunc, mallocArg);
  Value *castVal = Builder.CreateBitCast(val, PointerType::getUnqual(ty));

  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    Value *gep = Builder.CreateStructGEP(castVal, i);
    Value *val = Args[i]->Codegen();
    Builder.CreateStore(val, gep);
  }

  return castVal;
}

Value *Register::Codegen() {
  if (Alloca == NULL) {
    Value *InitVal = Source->Codegen();
    //Function *TheFunction = Builder.GetInsertBlock()->getParent();
    //IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
    //  TheFunction->getEntryBlock().begin());
    Alloca = Builder.CreateAlloca(Source->getType(), 0, Name.c_str());

    Builder.CreateStore(InitVal, Alloca);
  }

  return Alloca;
}

Value *RegisterFunArg::Codegen() {
  assert(Alloca != NULL);
  return Alloca;
}

Value *Binding::Codegen() {
  InitReg->Codegen();
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

Value *While::Codegen() {

  Function *TheFunction = Builder.GetInsertBlock()->getParent();
  BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "While", TheFunction);
  Builder.CreateBr(LoopBB);
  Builder.SetInsertPoint(LoopBB);
  
  Body->Codegen();
  
  // Emit the step value.
  Value *EndCond = Cond->Codegen();

  BasicBlock *LoopEndBB = Builder.GetInsertBlock();
  BasicBlock *AfterBB = BasicBlock::Create(
    getGlobalContext(), "afterloop", TheFunction);
  
  Builder.CreateCondBr(EndCond, LoopBB, AfterBB);
  
  Builder.SetInsertPoint(AfterBB);
  
  return NULL;
}

void Closure::getArgRegs(vector<RegisterFunArg*> &args) {
  vector<RegisterFunArg*> funArgRegs;
  FuncRef->getArgRegs(funArgRegs);
  args.assign(funArgRegs.begin() + ActivationRecord.size(), funArgRegs.end());
}

vector<Expr*> *Closure::getActivationRecord() { return &ActivationRecord; }

Func *Call::getFunc() {
  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    return cl->getFunc();
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
    Func *fn = cl->getFunc();
    vector<Expr*> *rec = cl->getActivationRecord();
    for (unsigned i=0, e=rec->size(); i != e; ++i)
      args.push_back((*rec)[i]);
  }
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    args.push_back(Args[i]);
}

void Call::getGenerics(vector<SType*> &tys) {
  vector<SType*> callTypes;
  vector<Expr*> args;
  getAllArgs(args);
  for (unsigned i=0, e=args.size(); i != e; ++i)
    callTypes.push_back(args[i]->getSType());

  vector<SType*> genericBindings;
  getFunc()->MatchGenerics(callTypes, genericBindings);

  for (unsigned i=0, e=genericBindings.size(); i != e; ++i)
    if (SGenericType *ty = dynamic_cast<SGenericType*>(genericBindings[i]))
      tys.push_back(ty->getBinding());
    else
      tys.push_back(genericBindings[i]);
}

Value *Call::Codegen() {
  vector<Value*> argVals;

  if (Closure *cl = dynamic_cast<Closure*>(Callee)) {
    // First arguments to call are the closure's activation record.
    vector<Expr*> *ar = cl->getActivationRecord();
    for (unsigned i=0, e=ar->size(); i != e; ++i)
      argVals.push_back((*ar)[i]->Codegen());
  }

  for (unsigned i=0, e=Args.size(); i != e; ++i)
    argVals.push_back(Args[i]->Codegen());

  Func *fn = getFunc();

  vector<SType*> genericBindings;
  getGenerics(genericBindings);
  vector<SType*> oldGenericBindings;
  fn->getGenerics(oldGenericBindings);
  fn->setGenerics(genericBindings);

  Value *val = Builder.CreateCall(
    fn->getFunction(), argVals.begin(), argVals.end(), "calltmp");

  fn->setGenerics(oldGenericBindings);

  return val;
}

Value *Closure::Codegen() {
  // XXX
  // XXX: serious problem here, a closure can refer to a generic function
  // that is specialized in two ways. Need to have the right reference here.
  // XXX
  // XXX
  return FuncRef->getFunction();
}

Function *Extern::getFunction() {
  Function *func = TheModule->getFunction(GetName());
  if (func == NULL) {
    FunctionType const *ft = getFunctionSType()->getFunctionType();
    func = Function::Create(ft, Function::ExternalLinkage, GetName(), TheModule);
  }
  return func;
}

// Can only be called after Bind phase.
Function *Func::getFunction() {
  vector<SType*> genericBindings;
  getGenerics(genericBindings);

  if (functions.count(genericBindings) > 0)
    return functions[genericBindings];

  FunctionType const *ft = getFunctionSType()->getFunctionType();
  //std::cerr << "FunctionType " << Name << " type: ";
  //ft->dump();
  //std::cerr << std::endl;

  string name;
  getFullName(name);
  Function *function =
    Function::Create(ft, Function::ExternalLinkage, name, TheModule);

  if (function->getName() != name) {
    std::cerr << "Function redeclaration: `" << name << "'\n";
    exit(1);
  }

  unsigned idx = 0;
  for (Function::arg_iterator ai=function->arg_begin(); idx != Args.size();
      ++ai,++idx)
    ai->setName(Args[idx]);

  functions[genericBindings] = function;

  return function;
}

void Func::getFullName(string &name) {
  name.assign(Name);

  for (unsigned i=0, e=GenericPlaceholders.size(); i != e; ++i) {
    name.append("$");
    SType *ty = GenericPlaceholders[i]->getBinding();
    if (ty != NULL)
      name.append(ty->getName());
  }
}

void Func::getGenerics(vector<SType*> &gens) {
  for (unsigned i=0, e=GenericPlaceholders.size(); i != e; ++i)
    gens.push_back(GenericPlaceholders[i]->getBinding());
}
bool Func::isGeneric() { return Generics.size() > 0; }
void Func::setGenerics(const vector<SType*> &tys) {
  assert(tys.size() == GenericPlaceholders.size());
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
  DEBUG(dbgs() << "Codegen: " << Name << "\n");

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
  if (llvm::DebugFlag)
    function->dump();
  verifyFunction(*function);

  return function;
}

void Func::MatchGenerics(
    const vector<SType*> &callTypes,
    vector<SType*> &genericBindings) {
  assert(callTypes.size() == Args.size());
  assert(genericBindings.size() == 0);
  map<SGenericType*, SType*> matchedGenerics;
  for (unsigned i=0, e=Args.size(); i != e; ++i)
    if (SGenericType* gen = dynamic_cast<SGenericType*>(ArgSTypes[i]))
      matchedGenerics[gen] = callTypes[i];
  for (unsigned i=0, e=GenericPlaceholders.size(); i != e; ++i)
    genericBindings.push_back(matchedGenerics[GenericPlaceholders[i]]);

  // As we are generating type signatures for specialization, all
  // boxed structures are equivalent to us, so we replace them with null.
  for (unsigned i=0, e=genericBindings.size(); i != e; ++i)
    if (SStructType *sty = dynamic_cast<SStructType*>(genericBindings[i]))
      if (!sty->isUnboxed())
        genericBindings[i] = NULL;
}

File::File(string &name,
    const vector<Func*> &funcs,
    const vector<Extern*> &externs,
    const vector<SType*> &tys)
  : Name(name),
    Funcs(funcs),
    Externs(externs),
    STypes(tys),
    FileModule(name, getGlobalContext()) {}

  /*
TODO: in JIT pass experiment with:

  FunctionPassManager fpm(&FileModule);
  fpm.add(new TargetData(*TheExecutionEngine->getTargetData()));
  fpm.doInitialization();
  iplist<Function> &fns = TheModule->getFunctionList();
  for (iplist<Function>::iterator it = fns.begin(); it != fns.end(); it++)
    fpm.run(*it);

*/

void File::optimize() {
  FunctionPassManager fpm(&FileModule);
  fpm.doInitialization();
  createStandardFunctionPasses(&fpm, 2);
  iplist<Function> &fns = TheModule->getFunctionList();
  for (iplist<Function>::iterator it = fns.begin(); it != fns.end(); it++)
    fpm.run(*it);

  PassManager pm;
  createStandardModulePasses(&pm, 2, false, false, true, true, false, NULL);
  pm.run(FileModule);
}

void File::compile() {
  string errStr;
  TheModule = &FileModule;

  DEBUG(dbgs() << "PHASE: Init\n");
  {
    const FunctionType *mallocTy = FunctionType::get(
      Type::getInt8PtrTy(getGlobalContext()),
      vector<const Type*>(1, Type::getInt64Ty(getGlobalContext())), false);
    const FunctionType *printTy = FunctionType::get(
      Type::getInt32Ty(getGlobalContext()),
      vector<const Type*>(1, SString::get()->getType()), false);

    Function *mallocFunc = TheModule->getFunction("GC_malloc");
    if (mallocFunc == 0)
      mallocFunc = Function::Create(
        mallocTy, Function::ExternalLinkage, "GC_malloc", TheModule);

    Function *mallocAtomicFunc = TheModule->getFunction("GC_malloc_atomic");
    if (mallocAtomicFunc == 0)
      mallocAtomicFunc = Function::Create(
        mallocTy, Function::ExternalLinkage, "GC_malloc_atomic", TheModule);

    Function *printFunc = TheModule->getFunction("print");
    if (printFunc == 0)
      printFunc = Function::Create(
        printTy, Function::ExternalLinkage, "print", TheModule);
  }

  DEBUG(dbgs() << "PHASE: LambdaLift\n");
  LambdaLiftFuncs();

  DEBUG(dbgs() << "PHASE: BindTypes\n");
  NamedTypes = SType::Builtins();
  /*
  NamedTypes["Bool"]  = new SBool();
  NamedTypes["Int8"]  = new Int8();
  NamedTypes["Int16"] = new Int16();
  NamedTypes["Int32"] = new Int32();
  NamedTypes["Int64"] = new Int64();
  NamedTypes["String"] = new SString();
  */
  for (vector<SType*>::const_iterator i=STypes.begin(); i!=STypes.end(); i++) {
    // TODO check for overloading primitive NamedTypes.count((*i)->getName())
    NamedTypes[(*i)->getName()] = *i;
  }
  for (vector<SType*>::const_iterator i=STypes.begin(); i!=STypes.end(); i++) {
    vector<string> ResolutionPath;
    (*i)->Bind(ResolutionPath, NamedTypes);
    if (llvm::DebugFlag)
      (*i)->getType()->dump();
  }
  // Load top-level function names into binding scope.
  map<string, Expr*> NamedExprs;
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++) {
    string name;
    (*i)->getFullName(name);
    NamedExprs[name] = *i;
  }
  for (unsigned i=0, e=Externs.size(); i != e; ++i)
    NamedExprs[Externs[i]->GetName()] = Externs[i];

  // PHASE: BindNames.
  DEBUG(dbgs() << "PHASE: BindNames\n");
  for (unsigned i=0, e=Externs.size(); i != e; ++i)
    Externs[i]->Bind(NamedExprs);
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
    (*i)->Bind(NamedExprs);

  // PHASE: Type inference.
  {
    DEBUG(dbgs() << "PHASE: TypeInference\n");
    for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++) {
      //std::cerr << "Type inference on: " << (*i)->getName() << std::endl;
      TypeInferer inferer;
      (*i)->TypeInfer(inferer);
      inferer.TypeUnification();
      inferer.TypePopulation();
    }
  }

  // PHASE: Specialization Codegen
  {
    DEBUG(dbgs() << "PHASE: Specialization Codegen\n");

    // Generate a call graph (using `FindCalls') from every non-generic
    // function, use this to generate specialized versions of generics.
    vector<pair<Func*,vector<SType*> > > calls;
    for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++)
      if (!(*i)->isGeneric())
        (*i)->FindCalls(calls);
    Util::removeDuplicates(calls);

    vector<pair<Func*,vector<SType*> > >::iterator it;
    for (it=calls.begin(); it != calls.end(); ++it) {
      if (!Util::allNull(it->second)) {
        Func *fn = it->first;
        fn->setGenerics(it->second);
        fn->Codegen();
        fn->clearGenerics();
      }
    }
  }

  // PHASE: Code generation
  DEBUG(dbgs() << "PHASE: Codegen\n");
  for (unsigned i=0, e=Externs.size(); i != e; ++i)
    Externs[i]->Codegen();
  for (vector<Func*>::const_iterator i=Funcs.begin(); i!=Funcs.end(); i++) {
    // TODO: pass in the LLVM state as an argument.
    (*i)->Codegen();
  }
}

}; };
