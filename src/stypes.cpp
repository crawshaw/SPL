#include "ast.h"
#include <iostream>
#include "llvm/LLVMContext.h"

using namespace llvm;

namespace SPL { namespace AST {

extern map<string,SType*> NamedTypes;

void SVoid::dump() { std::cerr << "SVoid::" << Name; }
void SPrimitive::dump() { std::cerr << "SPrimitive::" << Name; }

void SStructType::dump() {
  std::cerr << "SStructType::" << Name << " {" << std::endl;
  if (ElementSTypes.size() == 0) {
    std::cerr << "  (unbound)" << std::endl;
    for (unsigned i=0, e=ElementNames.size(); i != e; ++i)
      std::cerr << "  " << ElementNames[i] << " : "
        << ElementSTypeNames[i] << std::endl;
  } else {
    for (unsigned i=0, e=ElementNames.size(); i != e; ++i) {
      std::cerr << "  " << ElementNames[i] << " : ";
      ElementSTypes[i]->dump();
      std::cerr << std::endl;
    }
  }
  std::cerr << "}" << std::endl;
}

void SArray::dump()  { std::cerr << "SArray"; }
void SString::dump() { std::cerr << "SString"; }

void SFunctionType::dump() {
  std::cerr << "SFunctionType::" << Name << "(";
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    Args[i]->dump();
    std::cerr << ", ";
  }
  std::cerr << ") -> ";
  Ret->dump();
  std::cerr << std::endl;
}

void SPtr::dump() {
  std::cerr << "SPtr::";
  Ref->dump();
  std::cerr << std::endl;
}

void SGenericType::dump() {
  std::cerr << "SGenericType::" << Name;
}


/////////////////////////////////////////////////////////////////////


Type const* SVoid::getType() { return Type::getVoidTy( getGlobalContext()); }
Type const* Int8 ::getType() { return Type::getInt8Ty( getGlobalContext()); }
Type const* Int16::getType() { return Type::getInt16Ty(getGlobalContext()); }
Type const* Int32::getType() { return Type::getInt32Ty(getGlobalContext()); }
Type const* Int64::getType() { return Type::getInt64Ty(getGlobalContext()); }
void SStructType::Bind(
    vector<string> &ResolutionPath, map<string, SType*> &NamedTypes) {
  if (ElementSTypes.size() == ElementSTypeNames.size())
    return; // We are bound.
  for (unsigned i=0, e=ResolutionPath.size(); i != e; ++i) {
    if (ResolutionPath[i] == Name) {
      std::cerr << "Circular type dependency detected: " << std::endl;
      for (; i != e; ++i)
        std::cerr << "  " << ResolutionPath[i] << " --> " << std::endl;
      std::cerr << "  " << Name << std::endl;
      exit(1);
    }
  }

  ResolutionPath.push_back(Name);
  for (unsigned i=0, e=ElementSTypeNames.size(); i != e; ++i) {
    SType *ty = ElementSTypeNames[i]->Resolve(NamedTypes);
    if (ty == NULL) {
      std::cerr << "Unknown Type: " << ElementSTypeNames[i]
        << " in field of " << Name << std::endl;
      exit(1);
    }
    ty->Bind(ResolutionPath, NamedTypes);
    ElementSTypes.push_back(ty);
  }
  ResolutionPath.pop_back();

  vector<const Type *> tys;
  for (unsigned i=0, e=ElementSTypes.size(); i != e; ++i)
    tys.push_back(ElementSTypes[i]->getType());
  ThisType = StructType::get(getGlobalContext(), tys);
}
Type const *SStructType::getPassType() { return ThisType; }
Type const *SStructType::getType() { return PointerType::getUnqual(ThisType); }
void SArray::Bind(vector<string> &, map<string, SType*> &) {
  SType *ty = Contained;
  if (ty == NULL) {
    std::cerr << "Array type parameter undefined." << std::endl;
    exit(1);
  }

  // For show, fill these.
  ElementNames.push_back("length");
  ElementSTypeNames.push_back(new TypePlaceholder("Int32"));
  ElementNames.push_back("data");
  ElementSTypeNames.push_back(new TypePlaceholder(ty->getName()));

  vector<const Type *> tys;
  tys.push_back(Type::getInt32Ty(getGlobalContext()));
  tys.push_back(PointerType::getUnqual(ArrayType::get(ty->getType(), 0)));
  ThisType = StructType::get(getGlobalContext(), tys);
}
SType* SArray::ParamRebind(vector<SType*> &prms) {
  assert(prms.size() == 1);
  SArray *sty = new SArray(prms[0]);
  vector<string> x1;
  sty->Bind(x1, NamedTypes);
  return sty;
}

Type const *SFunctionType::getType() {
  return PointerType::getUnqual(getFunctionType());
}
FunctionType const *SFunctionType::getFunctionType() {
  vector<const Type*> ArgTypes;
  for (vector<SType*>::const_iterator i=Args.begin(); i!=Args.end(); i++)
    ArgTypes.push_back((*i)->getType());
  return FunctionType::get(Ret->getType(), ArgTypes, false);
}
SType* SFunctionType::ParamRebind(vector<SType*> &prms) {
  assert(prms.size() >= 2);
  vector<SType*> args(prms.begin(), prms.end() - 1);
  SType* ret = prms[prms.size() - 1];
  string name("Function");
  SFunctionType *sty = new SFunctionType(name, args, ret);
  vector<string> x1;
  sty->Bind(x1, NamedTypes);
  return sty;
}
void SFunctionType::MatchGenerics(
    const vector<SType*> &callTypes,
    vector<SType*> &genericBindings) {
  assert(callTypes.size() == Args.size());
  assert(genericBindings.size() == 0);
  map<SGenericType*, SType*> matchedGenerics;
  for (unsigned i=0, e=Args.size(); i != e; ++i) {
    if (SGenericType* gen = dynamic_cast<SGenericType*>(Args[i])) {
      matchedGenerics[gen] = callTypes[i];
      genericBindings.push_back(callTypes[i]);
    }
  }

  // As we are generating type signatures for specialization, all
  // boxed structures are equivalent to us, so we replace them with null.
  for (unsigned i=0, e=genericBindings.size(); i != e; ++i)
    if (SStructType *sty = dynamic_cast<SStructType*>(genericBindings[i]))
      if (!sty->isUnboxed())
        genericBindings[i] = NULL;
}

Type const *SPtr::getType() { return PointerType::getUnqual(Ref->getType()); }
Type const *SBool::getType() { return Type::getInt1Ty(getGlobalContext()); }
Type const *SGenericType::getType() {
  if (Binding == NULL) {
    return Type::getInt8PtrTy(getGlobalContext());
  } else {
    return Binding->getType();
  }
}

/////////////////////////////////////////////////////////////////////

SType *TypePlaceholder::Resolve(map<string,SType*> &NamedTypes) {
  vector<SType*> resolvedParams;
  for (unsigned i=0, e=Params.size(); i != e; ++i)
    resolvedParams.push_back(Params[i]->Resolve(NamedTypes));
  return NamedTypes[Name]->ParamRebind(resolvedParams);
}

SGenericType *TypePlaceholder::ResolveAsGeneric(map<string,SType*> &NamedTypes) {
  vector<SType*> resolvedParams;
  for (unsigned i=0, e=Params.size(); i != e; ++i)
    resolvedParams.push_back(Params[i]->Resolve(NamedTypes));
  return new SGenericType(Name, resolvedParams);
}


/////////////////////////////////////////////////////////////////////


unsigned SStructType::getIndex(const string &name) {
  for (unsigned i=0, e =ElementNames.size(); i != e; ++i)
    if (ElementNames[i] == name)
      return i;
  return -1;
}

map<string,SType*> SType::BuiltinsMap;
const map<string,SType*> &SType::Builtins() {
  if (BuiltinsMap.size() == 0) {
    BuiltinsMap["Bool"]  = new SBool();
    BuiltinsMap["Int8"]  = new Int8();
    BuiltinsMap["Int16"] = new Int16();
    BuiltinsMap["Int32"] = new Int32();
    BuiltinsMap["Int64"] = new Int64();
    BuiltinsMap["String"] = new SString();
    BuiltinsMap["Array"] = new SArray(NULL);
    BuiltinsMap["Function"] = new SFunctionType();
  }
  return BuiltinsMap;
}

SString *SString::Singleton = NULL;
SString* SString::get() {
  if (Singleton == NULL)
    Singleton = new SString();
  return Singleton;
}

Type *SArray::GenericTypeSingleton = NULL;
Type* SArray::GenericType() {
  if (GenericTypeSingleton == NULL) {
    const Type *i8 = Type::getInt8PtrTy(getGlobalContext());
    vector<const Type *> tys;
    tys.push_back(Type::getInt32Ty(getGlobalContext()));
    tys.push_back(PointerType::getUnqual(ArrayType::get(i8, 0)));
    GenericTypeSingleton = PointerType::getUnqual(
      StructType::get(getGlobalContext(), tys));
  }
  return GenericTypeSingleton;
}

}};
