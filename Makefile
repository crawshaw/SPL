test: build/spl
	@echo 'Some very basic tests:'
	echo '45 + 3' | ./$^
	echo 'def f(x,y,z) = x + y + z' | ./$^
	echo 'def f(x,y,z) = { z*x }' | ./$^
	echo 'imp f(x,y,z) = { z*x }' | ./$^
	echo 'io  f(x,y,z) = { z*x }' | ./$^
	echo 'def f(x,y) = if x == y then 1 else 0' | ./$^

build/grammar.cpp: src/grammar.y
	@mkdir -p build
	bison -o $@ $<

LLVMCONFIG=/Users/crawshaw/repo/llvm-2.7/Release/bin/llvm-config
LLVMFLAGS=$(shell $(LLVMCONFIG) --cxxflags)

build/spl: build/grammar.cpp src/codegen.cpp
	g++ $(LLVMFLAGS) -I src -o $@ $^

clean:
	rm -rf build
