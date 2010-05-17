test: build/spl
	@echo 'Running tests:'
	@for f in `ls tests/*.spl`; do echo "  $$f"; ./$^ $$f; done

build/grammar.cpp: src/ast.h
src/codegen.cpp: src/ast.h

build/grammar.cpp: src/grammar.y
	@mkdir -p build
	bison -o $@ $<

build/spl: build/grammar.cpp src/codegen.cpp src/lambdalift.cpp
	clang++ `llvm-config --cxxflags --ldflags --libs` -frtti -I src -g -o $@ $^

clean:
	rm -rf build
