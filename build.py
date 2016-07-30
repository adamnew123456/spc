import importlib
import os, os.path
import sys

from spc import driver, errors, lexer

class BuildFailure(Exception):
    pass

BUILD_ROOT = os.getcwd() + '/'
BACKENDS = ['linux_x86', 'mars_mips']

ASSEMBLERS = {
    'linux_x86': '/usr/bin/as',
    'mars_mips': None,
}

LINKERS = {
    'linux_x86': '/usr/bin/gold',
    'mars_mips': None,
}

# The core architecture libraries for each platform.
ARCH_LIBS = {
    'linux_x86': BUILD_ROOT + 'arch/linux_x86.lisp',
    'mars_mips': BUILD_ROOT + 'arch/mars_mips.lisp',
}

# The source directories for each platform's samples
SOURCE_DIRS = {
    'linux_x86': BUILD_ROOT + 'samples/linux_x86',
    'mars_mips': BUILD_ROOT + 'samples/mars_mips',
}

# The build directories where output files are located for each platform
BUILD_DIRS = {
    'linux_x86': BUILD_ROOT + 'build/linux_x86',
    'mars_mips': BUILD_ROOT + 'build/mars_mips',
}

# The files that can be compiled in each platform, and their dependencies
TARGETS = {
    'linux_x86': {
        'arith.lisp': [
            BUILD_ROOT + 'lib/assert.lisp', 
            BUILD_ROOT + 'lib/io.lisp'],
        'cached-require.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'cmp.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'cond.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'error.lisp': [],
        'fizzbuzz.lisp': [
            BUILD_ROOT + 'lib/str.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'hanoi.lisp': [
            BUILD_ROOT + 'lib/str.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'hello-world.lisp': [
            BUILD_ROOT + 'lib/io.lisp'],
        'if.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'not.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'ptr-arith.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'read.lisp': [
            BUILD_ROOT + 'lib/io.lisp'],
        'require-hiding.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'static-if.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'switch.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'unaligned-args.lisp': []
    },
    'mars_mips': {
        'arith.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'cached-require.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'cmp.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'cond.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'error.lisp': [],
        'fizzbuzz.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'func-ptr.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'hanoi.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'hello-global.lisp': [
            BUILD_ROOT + 'lib/io.lisp'],
        'hello-local.lisp': [
            BUILD_ROOT + 'lib/io.lisp'],
        'jagged-struct.lisp': [],
        'linked-ints.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'many-param.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'not.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'pair-ints.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'print-ascii.lisp': [],
        'ptr-arith.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'require-hiding.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'static-if.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'switch.lisp': [
            BUILD_ROOT + 'lib/assert.lisp',
            BUILD_ROOT + 'lib/io.lisp'],
        'unaligned-args.lisp': []
    }
}

def compile_file(platform, source, target, library=False):
    """
    Compiles a single .lisp file into a .asm file.
    """
    backend_module = importlib.import_module('spc.backends.' + platform)

    with open(source) as source_file, open(target, 'w') as target_file:
        lex = lexer.Lexer(source_file, source_file.name)
        backend = backend_module.get_backend(target_file, source_file.name, library)
        drv = driver.Driver(lex, backend)

        try:
            drv.compile()
        except errors.CompilerError as err:
            print(err.message, file=sys.stderr)
            raise BuildFailure

def assemble_file(assembler, input_file, output_file):
    """
    Assembles an assembly file to an object file.
    """
    result = os.system('{} "{}" -o "{}"'.format(assembler, input_file, output_file))
    if result != 0:
        raise BuildFailure

def link_files(linker, input_files, output_file):
    """
    Links together a series of object files to an executable file.
    """
    input_list = ' '.join('"' + filename + '"' for filename in input_files)
    result = os.system('{} {} -o "{}"'.format(linker, input_list, output_file))
    if result != 0:
        raise BuildFailure

def source_file_to_asm_file(platform, filename):
    """
    Converts a source path into the path of a compilation target (ASM file).

    >>> source_file_to_asm_file('linux_x86', 'lib/io.lisp')
    'build/linux_x86/io.asm'
    """
    _, just_name = os.path.split(filename)
    no_ext, _ = os.path.splitext(just_name)

    return BUILD_DIRS[platform] + '/' + no_ext + '.asm'

def replace_extension(filename, ext):
    """
    Replaces the extension on a file, retaining the path.

    >>> replace_extension(
    """
    no_ext, _ = os.path.splitext(filename)
    return no_ext + ext

def main(platform, filename):
    if platform not in BACKENDS:
        print('Invalid platform:', platform)
        for platform in BACKENDS:
            print(' -', platform)

        raise BuildFailure

    if filename not in TARGETS[platform]:
        print('Invalid target:', filename)
        for target in TARGETS[platform]:
            print(' -', target)

        raise BuildFailure

    os.makedirs(BUILD_DIRS[platform], exist_ok=True)

    main_source = SOURCE_DIRS[platform] + '/' + filename
    main_asm = BUILD_DIRS[platform] + '/' + replace_extension(filename, '.asm')
    compile_file(platform, main_source, main_asm)

    for dep in TARGETS[platform][filename]:
        dep_asm = source_file_to_asm_file(platform, dep)
        compile_file(platform, dep, dep_asm, library=True)

    arch_lib_source = ARCH_LIBS[platform]
    arch_lib_asm = source_file_to_asm_file(platform, arch_lib_source)
    compile_file(platform, arch_lib_source, arch_lib_asm, library=True)

    if ASSEMBLERS[platform] is not None:
        assembler = ASSEMBLERS[platform]

        main_obj = replace_extension(main_asm, '.o')
        assemble_file(assembler, main_asm, main_obj)

        for dep in TARGETS[platform][filename]:
            dep_asm = source_file_to_asm_file(platform, dep)
            dep_obj = replace_extension(dep_asm, '.o')
            assemble_file(assembler, dep_asm, dep_obj)


        arch_lib_obj = replace_extension(arch_lib_asm, '.o')
        assemble_file(assembler, arch_lib_asm, arch_lib_obj)

    if LINKERS[platform] is not None:
        linker = LINKERS[platform]
        main_bin = replace_extension(main_obj, '')

        inputs = []
        inputs.append(main_obj)

        for dep in TARGETS[platform][filename]:
            dep_asm = source_file_to_asm_file(platform, dep)
            dep_obj = replace_extension(dep_asm, '.o')
            inputs.append(dep_obj)

        inputs.append(arch_lib_obj)

        link_files(linker, inputs, main_bin)

if __name__ == '__main__':
    try:
        _, platform, filename = sys.argv
    except ValueError:
        print('build.py <PLATFORM> <FILENAME>', file=sys.stderr)
        sys.exit(1)

    try:
        main(sys.argv[1], sys.argv[2])
    except BuildFailure:
        sys.exit(1)

    sys.exit(0)
