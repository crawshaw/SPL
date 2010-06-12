#include <string>
#include <iostream>
#include "llvm/LLVMContext.h"
#include "llvm/Module.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Target/TargetSelect.h"

#include <dlfcn.h>

using namespace llvm;

cl::list<std::string> InputFilenames(
  cl::Positional, cl::desc("<program>"), cl::OneOrMore);

extern "C" void *print;
extern "C" void *println;
void *FindVMFunc(const std::string &name) {
  if (name == "print")
    return print;
  else if (name == "println")
    return println;
  else
    return NULL;
}

int main(int argc, char** argv) {
  EnableDebugBuffering = true;
  cl::ParseCommandLineOptions(argc, argv, "Sprint VM");

  std::string err;
  MemoryBuffer *in = MemoryBuffer::getFile(InputFilenames[0], &err);
  if (!err.empty()) {
    std::cerr << "Unable to open input file: " << InputFilenames[0] << std::endl;
    std::cerr << "Error: " << err << std::endl;
    exit(1);
  }

  Module *module = ParseBitcodeFile(in, getGlobalContext(), &err);
  if (!err.empty()) {
    std::cerr << "Error parsing module: " << err << std::endl;
    exit(1);
  }

  InitializeNativeTarget();
  ExecutionEngine * engine = EngineBuilder(module).setErrorStr(&err).create();
  if (!engine) {
    std::cerr << "ExecutionEngine: " << err << std::endl;
    exit(1);
  }

  // Something is wrong with the dynamic linker, so I provide my own.
  engine->InstallLazyFunctionCreator(FindVMFunc);
  //std::cerr << "symbolSearching disabled: " << engine->isSymbolSearchingDisabled()
  //  << std::endl;

  // Execute the function `main'.
  Function *f = module->getFunction("main");
  if (f == NULL) {
    std::cout << "main is not defined!" << std::endl;
    exit(1);
  }
  void *fptr = engine->getPointerToFunction(f);
  int32_t (*fp)() = (int32_t (*)())(intptr_t)fptr;
  std::cout << "Result: " << fp() << std::endl;
}
