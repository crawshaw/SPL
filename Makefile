test: build/spl
	@echo 'Some very basic tests:'
	echo '45 + 3' | ./$^
	echo 'def f(x,y,z) = x + y + z' | ./$^
	echo 'def f(x,y,z) = { z*x }' | ./$^
	echo 'imp f(x,y,z) = { z*x }' | ./$^
	echo 'io  f(x,y,z) = { z*x }' | ./$^
	echo 'def f(x,y) = if x == y then 1 else 0' | ./$^

build/grammar.c: src/grammar.y
	@mkdir -p build
	bison -o $@ $^

build/spl: build/grammar.c
	g++ -I src -o $@ $<

clean:
	rm -rf build
