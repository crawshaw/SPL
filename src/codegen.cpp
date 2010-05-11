#include "ast.h"
using namespace llvm;

namespace SPL { namespace AST {

Value *Number::Codegen() {
  return NULL;
}

Value *Variable::Codegen() {
  return NULL;
}

Value *Not::Codegen() {
  return NULL;
}

Value *Add::Codegen() {
  return NULL;
}

Value *Subtract::Codegen() {
  return NULL;
}

Value *Multiply::Codegen() {
  return NULL;
}

Value *Eq::Codegen() {
  return NULL;
}

Value *Seq::Codegen() {
  return NULL;
}

Value *Val::Codegen() {
  return NULL;
}

Value *If::Codegen() {
  return NULL;
}

Value *Call::Codegen() {
  return NULL;
}

Value *Function::Codegen() {
  return NULL;
}

Value *File::Codegen() {
  return NULL;
}

}; };
