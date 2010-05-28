CXX := clang++ `llvm-config --cxxflags` -frtti -I src -g -c

test: build/spl
	@echo 'Running tests:'
	@for f in `ls tests/*.spl`; do echo "  $$f"; ./$^ $$f; done

build/grammar.cpp: src/ast.h
src/codegen.cpp: src/ast.h
src/typeinference.cpp: src/ast.h
src/stypes.cpp: src/ast.h

build/grammar.cpp: src/grammar.y
	@mkdir -p build
	bison -o $@ $<

build/spl: build/grammar.o build/codegen.o build/lambdalift.o build/typeinference.o build/stypes.o
	clang++ -g $^ `llvm-config --ldflags --libs core jit native` -o $@

build/grammar.o: build/grammar.cpp
	$(CXX) -o $@ $<

build/codegen.o: src/codegen.cpp
	$(CXX) -o $@ $<

build/lambdalift.o: src/lambdalift.cpp
	$(CXX) -o $@ $<

build/typeinference.o: src/typeinference.cpp
	$(CXX) -o $@ $<

build/stypes.o: src/stypes.cpp
	$(CXX) -o $@ $<

clean:
	rm -rf build
