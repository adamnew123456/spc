clean:
	rm -rf build

## mars_mips TARGET ##

mars_mips_STDLIB := build/mars_mips/str.asm build/mars_mips/io.asm build/mars_mips/assert.asm

build/.mars_mips:
	mkdir -p build/mars_mips
	touch build/.mars_mips

build/mars_mips/assert.asm: build/.mars_mips lib/assert.lisp
	python3 compile.py -l -b mars_mips -o build/mars_mips/assert.asm lib/assert.lisp

build/mars_mips/io.asm: build/.mars_mips lib/io.lisp
	python3 compile.py -l -b mars_mips -o build/mars_mips/io.asm lib/io.lisp

build/mars_mips/str.asm: build/.mars_mips lib/str.lisp
	python3 compile.py -l -b mars_mips -o build/mars_mips/str.asm lib/str.lisp

build/mars_mips/mars_mips.asm: build/.mars_mips arch/mars_mips.lisp
	python3 compile.py -l -b mars_mips -o build/mars_mips/mars_mips.asm arch/mars_mips.lisp

build/mars_mips/%.asm: samples/mars_mips/%.lisp ${mars_mips_STDLIB} build/mars_mips/mars_mips.asm
	python3 compile.py -b mars_mips -o $@ $<

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
