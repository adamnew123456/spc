clean:
	rm build/*.asm

build/mars.asm: arch/mars.lisp
	python3 compile.py -l -b mars -o build/mars.asm arch/mars.lisp

build/%.asm: samples/%.lisp build/mars.asm
	python3 compile.py -b mars -o $@ $<
