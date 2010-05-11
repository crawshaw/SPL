test: build/spl
	echo '45 + 3' | $^
	echo 'def f(x,y,z)' | $^

build/grammar.c: src/grammar.y
	@mkdir -p build
	bison -o $@ $^

build/spl: build/grammar.c
	g++ -I src -o $@ $<

clean:
	rm -rf build
