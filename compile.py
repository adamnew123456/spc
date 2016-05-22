import getopt
import importlib
import sys

from spc import driver, errors, lexer

BACKEND = None
IN_FILE = sys.stdin
OUT_FILE = sys.stdout

FLAGS, POS_ARGS = getopt.getopt(sys.argv, 'b:o:')
for opt, arg in FLAGS:
    if opt == '-b':
        try:
            BACKEND = importlib.import_module(arg, 'spc')
        except ImportError:
            print('Invalid backend:', arg, file=sys.stderr)
            sys.exit(1)
    elif opt == '-o':
        try:
            OUT_FILE = open(arg, 'w')
        except IOError:
            print('Cannot open output file:', arg, file=sys.stderr)
            sys.exit(1)

if BACKEND is None:
    print('-b argument required', file=sys.stderr)

if POS_ARGS:
    try:
        IN_FILE = open(POS_ARGS[0])
    except IOError:
        print('Cannot open input file:', POS_ARGS[0], file=sys.stderr)
        sys.exit(1)

with IN_FILE, OUT_FILE:
    lex = lexer.Lexer(FILE)
    lexed_lists = lexer.to_list(lex.lex())

    backend = BACKEND.get_backend(OUT_FILE)
    drv = driver.Driver(lexed_lists, backend)
    try:
        drv.compile()
    except CompilerError as err:
        print(err, file=sys.stderr)
        sys.exit(1)
