clean:
	rm -rf build

## mars TARGET ##

build/mars:
	mkdir -p build/mars

build/mars/mars.asm: build/mars arch/mars.lisp
	python3 compile.py -l -b mars -o build/mars/mars.asm arch/mars.lisp

build/mars/%.asm: samples/mars/%.lisp build/mars/mars.asm
	python3 compile.py -b mars -o $@ $<

## linux_x86 TARGET ##

build/linux_x86:
	mkdir -p build/linux_x86

build/linux_x86/linux_x86.asm: build/linux_x86 arch/linux_x86.lisp
	python3 compile.py -l -b linux_x86 -o build/linux_x86/linux_x86.asm arch/linux_x86.lisp

build/linux_x86/%.asm: samples/linux_x86/%.lisp build/linux_x86/linux_x86.asm
	python3 compile.py -b linux_x86 -o $@ $<
