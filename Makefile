COMPILER_OBJS := \
  grammar.o codegen.o lambdalift.o typeinference.o stypes.o compiler.o
VM_OBJS := print.o vm.o
REPL_OBJS := \
  grammar.o codegen.o lambdalift.o typeinference.o stypes.o

CXX := clang++
CXXFLAGS := `llvm-config --cxxflags` -frtti -I src -g -c

test: build/splc build/splvm
	@echo 'Running tests'
	@for f in `ls tests/*.spl`; do \
    echo "----- $$f -----"; \
    ./build/splc $$f && ./build/splvm junk.bc; \
  done

build/grammar.cpp: src/ast.h
src/codegen.cpp: src/ast.h
src/typeinference.cpp: src/ast.h
src/stypes.cpp: src/ast.h
src/compiler.cpp: src/ast.h

build/grammar.cpp: src/grammar.y
	@mkdir -p build
	bison -o $@ $<

build/splc: $(COMPILER_OBJS:%=build/%)
	$(CXX) -g $^ `llvm-config --ldflags --libs` -o $@

build/splvm: $(VM_OBJS:%=build/%)
	$(CXX) -g -ldl $^ `llvm-config --ldflags --libs core jit native bitreader` -lgc -o $@

build/grammar.o: build/grammar.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<
build/%.o: src/%.cpp
	$(CXX) $(CXXFLAGS) -o $@ $<
build/%.bc: src/%.ll
	llvm-as -o $@ $<

build/%.s: src/%.ll
	llc -o $@ $< 
build/%.o: build/%.s
	$(CXX) -c -o $@ $<

clean:
	rm -rf build
