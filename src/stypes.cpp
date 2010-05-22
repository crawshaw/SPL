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


/////////////////////////////////////////////////////////////////////


Type const* Int8 ::getType() { return Type::getInt8Ty( getGlobalContext()); }
Type const* Int16::getType() { return Type::getInt16Ty(getGlobalContext()); }
Type const* Int32::getType() { return Type::getInt32Ty(getGlobalContext()); }
Type const* Int64::getType() { return Type::getInt64Ty(getGlobalContext()); }
void SStructType::Bind(map<string, SType*> &NamedTypes) {
  for (unsigned i=0, e=ElementSTypeNames.size(); i != e; ++i)
    ElementSTypes.push_back(NamedTypes[ElementSTypeNames[i]]);
  vector<const Type *> tys;
  for (unsigned i=0, e=ElementSTypes.size(); i != e; ++i)
    tys.push_back(ElementSTypes[i]->getType());
  ThisType = StructType::get(getGlobalContext(), tys);
}
Type const *SStructType::getPassType() { return ThisType; }
Type const* SStructType::getType() { return PointerType::getUnqual(ThisType); }
Type const *SFunctionType::getType() { return getFunctionType(); }
Type const *SPtr::getType() { return PointerType::getUnqual(Ref->getType()); }
Type const *SBool::getType() { return Type::getInt1Ty(getGlobalContext()); }


}};