#include "ast.h"
#include <iostream>
#include <fstream>
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/raw_ostream.h"

using namespace SPL;
namespace cl=llvm::cl;

extern int yyparse();
extern std::istream *codein;
extern std::vector<AST::Func*>      toplevel;
extern std::vector<AST::Extern*>    externs;
extern std::vector<AST::SType*>     types;
extern int line;
extern int col;

cl::list<std::string> InputFilenames(
  cl::Positional, cl::desc("<input file>"), cl::OneOrMore);
cl::opt<std::string> OutputFilename(
  "o", cl::desc("Output file name"), cl::value_desc("file"), cl::Prefix);
cl::opt<bool> Optimize("O", cl::desc("Optimize"));

int main(int argc, char** argv)
{
  llvm::EnableDebugBuffering = true;
  llvm::cl::ParseCommandLineOptions(argc, argv, "Sprint Compiler");

  std::string outFile(
    OutputFilename.empty() ? std::string("junk.bc") : OutputFilename);

  std::string fileName(InputFilenames[0]);
  std::ifstream infile(fileName.c_str());
  if (!infile.is_open()) {
    std::cerr << "Unable to open file: " << fileName << std::endl;
    exit(1);
  }
  codein = &infile;
  line = 1;
  col = 0;

  int ret = yyparse();
  if (ret)
    return ret;

  AST::File file(fileName, toplevel, externs, types);

  file.compile();

  if (Optimize)
    file.optimize();

  std::string err;
  llvm::raw_fd_ostream out(outFile.c_str(), err);
  if (!err.empty()) {
    std::cerr << "Unable to open output file: " << outFile << std::endl;
    exit(1);
  }
  llvm::WriteBitcodeToFile(&file.getModule(), out);

  return 0;
}

