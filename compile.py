import getopt
import importlib
import sys

from spc import driver, errors, lexer

USAGE = 'python3 compile.py -b BACKEND [-h] [-l] [-o OUTPUT] [INPUT]'
HELP = '''python3 compile.py -b BACKEND [-h] [-l] [-o OUTPUT] [INPUT]

Arguments:

    -h
        Shows this help page.

    -b BACKEND
        Required. This is the compiler backend. Currently accepted values are 'mars'.

    -l
        If given, this enables library mode - library mode prevents the compiler
        from emitting code which starts executing the program, making it suitable
        for modules which are not standalone.

        If not given, then the output is assumed to be executable, and 
        startup code is emitted.

    -o OUTPUT
        If given, the name of the output file where compiled code will be written.
        By default, the compiler writes to standard output.

    INPUT
        If given, the name of the input file where source code will be read from.
        By default, the compiler reads from standard input.
'''

BACKEND = None
LIBRARY = False
IN_FILE = sys.stdin
OUT_FILE = sys.stdout

FLAGS, POS_ARGS = getopt.getopt(sys.argv[1:], 'hlb:o:')
for opt, arg in FLAGS:
    if opt == '-h':
        print(HELP)
        sys.exit(0)
    elif opt == '-b':
        try:
            BACKEND = importlib.import_module('spc.backends.' + arg)
        except ImportError as err:
            print('Invalid backend:', arg, '(Reason: {})'.format(err), file=sys.stderr)
            sys.exit(1)
    elif opt == '-o':
        try:
            OUT_FILE = open(arg, 'w')
        except IOError as err:
            print('Cannot open output file:', arg, file=sys.stderr)
            sys.exit(1)
    elif opt == '-l':
        LIBRARY = True

if BACKEND is None:
    print(USAGE, file=sys.stderr)
    sys.exit(1)

if POS_ARGS:
    try:
        IN_FILE = open(POS_ARGS[0])
    except IOError:
        print('Cannot open input file:', POS_ARGS[0], file=sys.stderr)
        sys.exit(1)

with IN_FILE, OUT_FILE:
    if IN_FILE is sys.stdin:
        filename = '<stdin>'
    else:
        filename = IN_FILE.name

    lex = lexer.Lexer(IN_FILE, filename)
    backend = BACKEND.get_backend(OUT_FILE, filename, LIBRARY)
    drv = driver.Driver(lex, backend)
    try:
        drv.compile()
    except errors.CompilerError as err:
        print(err.message, file=sys.stderr)
        sys.exit(1)
