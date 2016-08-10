import copy
import importlib
import os, os.path
import sys

from spc import driver, errors, lexer

class BuildFailure(Exception):
    pass

BACKENDS = ['linux_x86', 'mars_mips']

ASSEMBLERS = {
    'linux_x86': '/usr/bin/as',
    'mars_mips': None,
}

LINKERS = {
    'linux_x86': '/usr/bin/gold',
    'mars_mips': None,
}

# The core architecture libraries for each backend.
ARCH_LIBS = {
    'linux_x86': 'arch/linux_x86.lisp',
    'mars_mips': 'arch/mars_mips.lisp',
}

# The build directories where output files are located for each backend. 
BUILD_DIRS = {
    'linux_x86': 'build/linux_x86',
    'mars_mips': 'build/mars_mips',
}

def exists(iterable):
    """
    Returns True if there is an element in the iterable, False otherwise.
    """
    try:
        next(iterable)
        return True
    except StopIteration:
        return False

def get_backend(filename):
    """
    Gets the backend for the file name, if there is one.
    """
    for backend in BACKENDS:
        if backend in filename:
            return backend

    return None

def parse_dependencies(fobj):
    """
    Parses the dependencies found by reading the given file object.

    Returns a dict which maps filenames to lists of filenames.
    """
    database = {}
    for line in fobj:
        line = line.strip()
        if not line:
            continue

        filename, raw_deps = line.split(':')
        backend = get_backend(filename)

        def replace_backend(dependency):
            if dependency == '*':
                if backend is None:
                    raise SyntaxError('Cannot use * without backend: ' + filename)
                else:
                    return ARCH_LIBS[backend]
            else:
                return dependency

        database[filename] = set(replace_backend(dep) for dep in raw_deps.split())

    return database

def build_dependencies(filename, database):
    """
    Builds a dependency list for the given filename.
    """
    deps = set()
    to_search = [filename]

    # First, prune out any irrelevant parts of the dependency mapping, by
    # figuring out what files are in the dependency chain
    while to_search:
        dep = to_search.pop()
        deps.add(dep)

        for sub_dep in database[dep]:
            if sub_dep not in deps:
                to_search.append(sub_dep)

    # Then, do the normal dependency sort
    relevant_db = {}
    for relevant_dep in deps:
        relevant_db[relevant_dep] = database[relevant_dep].copy()

    ordered_deps = []
    while exists(key for key in relevant_db if not relevant_db[key]):
        empty_key = next(key for key in relevant_db if not relevant_db[key])
        ordered_deps.append(empty_key)

        del relevant_db[empty_key]
        for nonempty_key in relevant_db:
            if empty_key in relevant_db[nonempty_key]:
                relevant_db[nonempty_key].remove(empty_key)

    if relevant_db:
        print('Cycle in dependency graph')
        for filename, deps in relevant_db:
            print(filename)
            for dep in deps:
                print(' -', dep)

        raise BuildFailure

    return ordered_deps
        
# The files that can be compiled in each backend, and their dependencies.
# Computed later on, in the __main__ block
TARGETS = {}

def compile_file(backend, source, target, library=False):
    """
    Compiles a single .lisp file into a .asm file.
    """
    backend_module = importlib.import_module('spc.backends.' + backend)

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

def source_to_asm_path(backend, filename):
    """
    Converts a source code filename into an assembly output filename.

    >>> source_to_asm_path('linux_x86', 'a/b/c/foo.lisp')
    'build/linux_x86/foo.asm'
    """
    _, source_file = os.path.split(filename)
    source_prefix, _ = os.path.splitext(source_file)
    return os.path.join(BUILD_DIRS[backend], source_prefix + '.asm')

def source_to_obj_path(backend, filename):
    """
    Converts a source code filename into an assembly output filename.

    >>> source_to_obj_path('linux_x86', 'a/b/c/foo.lisp')
    'build/linux_x86/foo.o'
    """
    _, source_file = os.path.split(filename)
    source_prefix, _ = os.path.splitext(source_file)
    return os.path.join(BUILD_DIRS[backend], source_prefix + '.o')

def source_to_bin_path(backend, filename):
    """
    Converts a source code filename into an assembly output filename.

    >>> source_to_bin_path('linux_x86', 'a/b/c/foo.lisp')
    'build/linux_x86/foo'
    """
    _, source_file = os.path.split(filename)
    source_prefix, _ = os.path.splitext(source_file)
    return os.path.join(BUILD_DIRS[backend], source_prefix)

def main(backend, filename):
    if filename not in TARGETS:
        print('Invalid target:', filename)
        for target in TARGETS:
            if get_backend(target) is not None and target.startswith('samples'):
                print(' -', target)

        raise BuildFailure

    os.makedirs(BUILD_DIRS[backend], exist_ok=True)

    source_files = build_dependencies(filename, TARGETS)
    for source_file in source_files:
        source_asm = source_to_asm_path(backend, source_file)
        compile_file(backend, source_file, source_asm, library=source_file != filename)

    if ASSEMBLERS[backend] is not None:
        assembler = ASSEMBLERS[backend]
        for source_file in source_files:
            source_asm = source_to_asm_path(backend, source_file)
            source_obj = source_to_obj_path(backend, source_file)
            assemble_file(assembler, source_asm, source_obj)

    if LINKERS[backend] is not None:
        linker = LINKERS[backend]
        main_bin = source_to_bin_path(backend, filename)

        inputs = []
        for source_file in reversed(source_files):
            inputs.append(source_to_obj_path(backend, source_file))

        link_files(linker, inputs, main_bin)

if __name__ == '__main__':
    # Add special support for the demo script
    with open('build.deps') as deps:
        TARGETS = parse_dependencies(deps)

    try:
        _, filename = sys.argv
    except ValueError:
        print('build.py <FILENAME>', file=sys.stderr)
        print('build.py sample.lisp')
        sys.exit(1)

    try:
        backend = get_backend(filename)
        if backend is None:
            print('Cannot build:', filename)

        main(backend, filename)
    except BuildFailure:
        sys.exit(1)

    sys.exit(0)
