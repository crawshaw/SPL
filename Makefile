test: build/spl
	@echo 'Running tests:'
	@for f in `ls tests/*.spl`; do echo "  $$f"; cat $$f | ./$^; done

build/grammar.cpp: src/ast.h
src/codegen.cpp: src/ast.h

build/grammar.cpp: src/grammar.y
	@mkdir -p build
	bison -o $@ $<

build/spl: build/grammar.cpp src/codegen.cpp src/lambdalift.cpp
	llvm-g++ `llvm-config --cxxflags --ldflags --libs` -I src -o $@ $^

clean:
	rm -rf build
