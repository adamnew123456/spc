clean:
	rm build/*.asm

build/mars.asm: arch/mars.asm
	cp arch/mars.asm build

build/%.asm: samples/%.lisp build/mars.asm
	python3 compile.py -b mars -o $@ $<
