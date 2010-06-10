#include "ast.h"
#include <iostream>
#include "llvm/LLVMContext.h"

using namespace llvm;

namespace SPL { namespace AST {

void SPrimitive::dump() {
  std::cerr << "SPrimitive::" << Name;
}

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

SString *SString::Singleton = NULL;

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


Type const* Int8 ::getType() { return Type::getInt8Ty( getGlobalContext()); }
Type const* Int16::getType() { return Type::getInt16Ty(getGlobalContext()); }
Type const* Int32::getType() { return Type::getInt32Ty(getGlobalContext()); }
Type const* Int64::getType() { return Type::getInt64Ty(getGlobalContext()); }
void SStructType::Bind(
    vector<string> &ResolutionPath, const map<string, SType*> &NamedTypes) {
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
    SType *ty = NamedTypes.find(ElementSTypeNames[i])->second;
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
void SArray::Bind(vector<string> &, const map<string, SType*> &) {}
SArray::SArray(SType *ty): SStructType("Array") {
  // For show, fill these.
  ElementNames.push_back("length");
  ElementSTypeNames.push_back("Int32");
  ElementNames.push_back("data");
  ElementSTypeNames.push_back(ty->getName());

  vector<const Type *> tys;
  tys.push_back(Type::getInt32Ty(getGlobalContext()));
  tys.push_back(PointerType::getUnqual(ArrayType::get(ty->getType(), 0)));
  ThisType = StructType::get(getGlobalContext(), tys);
}
Type const *SFunctionType::getType() { return getFunctionType(); }
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


unsigned SStructType::getIndex(const string &name) {
  for (unsigned i=0, e =ElementNames.size(); i != e; ++i)
    if (ElementNames[i] == name)
      return i;
  return -1;
}


}};
