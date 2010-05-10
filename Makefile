all: build/spl.o

build/spl.o: src/spl.cpp
	@mkdir -p build
	g++ -g -c -o $@ $^

clean:
	rm -rf build
