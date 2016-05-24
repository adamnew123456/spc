import getopt
import importlib
import sys

from spc import driver, errors, lexer

USAGE = 'python3 compile.py -b BACKEND [-o OUTPUT] [INPUT]'

BACKEND = None
IN_FILE = sys.stdin
OUT_FILE = sys.stdout

FLAGS, POS_ARGS = getopt.getopt(sys.argv[1:], 'hb:o:')
for opt, arg in FLAGS:
    if opt == '-h':
        print(USAGE)
        sys.exit(0)
    elif opt == '-b':
        try:
            BACKEND = importlib.import_module('spc.' + arg)
        except ImportError as err:
            print('Invalid backend:', arg, file=sys.stderr)
            sys.exit(1)
    elif opt == '-o':
        try:
            OUT_FILE = open(arg, 'w')
        except IOError as err:
            print('Cannot open output file:', arg, file=sys.stderr)
            sys.exit(1)

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
    lex = lexer.Lexer(IN_FILE)
    lexed_lists = lexer.to_list(lex.lex())

    backend = BACKEND.get_backend(OUT_FILE)
    drv = driver.Driver(lexed_lists, backend)
    try:
        drv.compile()
    except errors.CompilerError as err:
        print(err.message, file=sys.stderr)
        sys.exit(1)
