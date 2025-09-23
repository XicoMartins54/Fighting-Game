build:
	mkdir -p build
	ghc -outputdir build -o build/jogo app/*.hs

run: build
	./build/jogo

clean:
	rm -rf build
