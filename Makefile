COMPILER_OBJS := \
  grammar.o codegen.o lambdalift.o typeinference.o stypes.o compiler.o
VM_OBJS := vm.o
REPL_OBJS := \
  grammar.o codegen.o lambdalift.o typeinference.o stypes.o

CXX := clang++ `llvm-config --cxxflags` -frtti -I src -g -c

test: build/splc
	@echo 'Running tests:'
	@for f in `ls tests/*.spl`; do echo "-----"; echo "  $$f"; ./$^ $$f; done

build/grammar.cpp: src/ast.h
src/codegen.cpp: src/ast.h
src/typeinference.cpp: src/ast.h
src/stypes.cpp: src/ast.h
src/compiler.cpp: src/ast.h

build/grammar.cpp: src/grammar.y
	@mkdir -p build
	bison -o $@ $<

build/splc: $(COMPILER_OBJS:%=build/%)
	clang++ -g $^ `llvm-config --ldflags --libs` -o $@

build/splvm: $(VM_OBJS:%=build/%)
	clang++ -g $^ `llvm-config --ldflags --libs core jit native` -lgc -o $@

build/grammar.o: build/grammar.cpp
	$(CXX) -o $@ $<
build/%.o: src/%.cpp
	$(CXX) -o $@ $<

clean:
	rm -rf build
