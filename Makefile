clean:
	rm -rf build

## mars TARGET ##

mars_STDLIB := build/mars/str.asm build/mars/io.asm build/mars/assert.asm

build/.mars:
	mkdir -p build/mars
	touch build/.mars

build/mars/assert.asm: build/.mars lib/assert.lisp
	python3 compile.py -l -b mars -o build/mars/assert.asm lib/assert.lisp

build/mars/io.asm: build/.mars lib/io.lisp
	python3 compile.py -l -b mars -o build/mars/io.asm lib/io.lisp

build/mars/str.asm: build/.mars lib/str.lisp
	python3 compile.py -l -b mars -o build/mars/str.asm lib/str.lisp

build/mars/mars.asm: build/.mars arch/mars.lisp
	python3 compile.py -l -b mars -o build/mars/mars.asm arch/mars.lisp

build/mars/%.asm: samples/mars/%.lisp ${mars_STDLIB} build/mars/mars.asm
	python3 compile.py -b mars -o $@ $<

## linux_x86 TARGET ##

linux_x86_STDLIB := build/linux_x86/str.asm build/linux_x86/io.asm build/linux_x86/assert.asm

build/.linux_x86:
	mkdir -p build/linux_x86
	touch build/.linux_x86

build/linux_x86/assert.asm: build/.linux_x86 lib/assert.lisp
	python3 compile.py -l -b linux_x86 -o build/linux_x86/assert.asm lib/assert.lisp

build/linux_x86/io.asm: build/.linux_x86 lib/io.lisp
	python3 compile.py -l -b linux_x86 -o build/linux_x86/io.asm lib/io.lisp

build/linux_x86/str.asm: build/.linux_x86 lib/str.lisp
	python3 compile.py -l -b linux_x86 -o build/linux_x86/str.asm lib/str.lisp

build/linux_x86/linux_x86.asm: build/.linux_x86 arch/linux_x86.lisp
	python3 compile.py -l -b linux_x86 -o build/linux_x86/linux_x86.asm arch/linux_x86.lisp

build/linux_x86/%.asm: samples/linux_x86/%.lisp ${linux_x86_STDLIB} build/linux_x86/linux_x86.asm
	python3 compile.py -b linux_x86 -o $@ $<
