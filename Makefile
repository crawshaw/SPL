all: build/spl

build/grammar.c: src/grammar.y
	@mkdir -p build
	bison -o $@ $^

build/spl: build/grammar.c
	g++ -I src -o $@ $<

clean:
	rm -rf build
