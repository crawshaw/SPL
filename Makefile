test: build/spl
	echo '45 + 3' | $^
	echo 'def f(x,y,z) = x + y + z' | $^
	echo 'def f(x,y,z) = { z*x }' | $^

build/grammar.c: src/grammar.y
	@mkdir -p build
	bison -o $@ $^

build/spl: build/grammar.c
	g++ -I src -o $@ $<

clean:
	rm -rf build
