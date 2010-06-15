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
#include "llvm/Target/TargetData.h"
#include "llvm/PassManager.h"

#include <dlfcn.h>

using namespace llvm;

cl::list<std::string> InputFilenames(
  cl::Positional, cl::desc("<program>"), cl::OneOrMore);
cl::opt<bool> Optimize("O", cl::desc("Optimize"));

extern "C" void *print;
extern "C" void *length;
void *FindVMFunc(const std::string &name) {
  if (name == "print")
    return print;
  else if (name == "length")
    return length;
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

  if (Optimize) {
    // TODO: experiment, what exactly does target data optimization give us,
    //       do we need to run other optimization passes after it?
    FunctionPassManager fpm(module);
    fpm.add(new TargetData(*engine->getTargetData()));
    fpm.doInitialization();
    iplist<Function> &fns = module->getFunctionList();
    for (iplist<Function>::iterator it = fns.begin(); it != fns.end(); it++)
      fpm.run(*it);
  }

  // Execute the function `main'.
  Function *f = module->getFunction("main");
  if (f == NULL) {
    std::cout << "main is not defined!" << std::endl;
    exit(1);
  }
  void *fptr = engine->getPointerToFunction(f);
  int32_t (*fp)() = (int32_t (*)())(intptr_t)fptr;
  int32_t res = fp();
  std::cout << "Result: " << res << std::endl;
}
