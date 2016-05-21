"""
MARS compiler backend - responsible for taking program events emitted by the
driver and converting them into code.
"""
import logging

from .errors import CompilerError
from . import expressions
from . import types

LOGGER = logging.getLogger('spc.mars')

class SymbolTable:
    """
    A lookup table which binds variables in a nested way, where access goes
    from the current symbol table up to the parent.
    """
    def __init__(self, parent=None, is_global=False, is_builtin=False):
        self.parent = parent
        self.is_builtin = is_builtin
        self.is_global = is_global
        self.bindings = {}

    def find(self, key):
        """
        Finds the symbol table in which the given key is bound, or returns
        None if the symbol is not bound.
        """
        table = self
        while table is not None:
            if key in table.bindings:
                return table
            else:
                table = table.parent

        return None

    def __getitem__(self, key):
        table = self
        while table is not None:
            if key in table.bindings:
                return table.bindings[key]
            else:
                table = table.parent

        raise KeyError('Could not resolve symbol "{}"'.format(parent))

    def __setitem__(self, key, value):
        if key in self:
            raise KeyError('Cannot overwrite existing definition of "{}"'.format(key))

        self.bindings[key] = value

    def __contains__(sef, key):
        table = self
        while table is not None:
            if key in table.bindings:
                return True
            else:
                table = table.parent

        return False

# A context is a bundle of symbol tables for values, functions, and types
Context = namedtuple('Context', ['value_defns', 'type_defns', 'func_stack'])

BUILTIN_TYPES = SymbolTable(is_builtin=True)
BUILTIN_TYPES['string'] = types.PointerTo(types.Byte)

BUILTIN_FUNCTIONS = SymbolTable(is_builtin=True)
BUILTIN_FUNCTIONS['@print-int'] = types.FunctionDecl(types.Byte, (types.Integer,))
BUILTIN_FUNCTIONS['@print-string'] = types.FunctionDecl(types.Byte, (types.TypeName('string'),))
BUILTIN_FUNCTIONS['@read-int'] = types.FunctionDecl(types.Integer, ())
BUILTIN_FUNCTIONS['@read-string'] = types.FunctionDecl(types.Byte, (types.TypeName('string'), types.Integer))
BUILTIN_FUNCTIONS['@sbrk'] = types.FunctionDecl(types.AnyPoniter, (types.Integer,))
BUILTIN_FUNCTIONS['@exit'] = types.FunctionDecl(types.Byte, ())

class FunctionStack:
    """
    Tracks where variables are on the function's stack.
    """
    # The stack discipline in use here works like this:
    #
    #     +-------
    #     | arg 2
    #     +------- offset 4
    #     | arg 1
    #     +------- offset 0
    #     | $fp
    #     +------- offset -4
    #     | $ra
    #     +------- offset -8
    #     | local 1
    #     +-------
    #     | local 2
    #     +-------
    #
    # Note that this doesn't take into account the calling convention, because
    # this compiler pretty much ignores it. All arguments are passed on the stack,
    # in reverse order. Functions keep all their local vars on the stack, to keep
    # them safe during recursion, and parameters are no exception - taking them
    # in via $a0 - $a3 would require an extra copy from the arg. register to memory

    def __init__(self):
        self.local_offset = -8
        self.param_offset = 0

    def add_param(self, name, size, alignment):
        """
        Adds a new parameter to the stack.
        """
        self.param_offset = types.align_address(self.param_offset, alignment)

        self.vars[name] = self.param_offset
        self.param_offset += size

    def add_local(self, name, size, alignment):
        """
        Adds a local variable to the stack.
        """
        self.local_offset = (
            types.align_address(self.local_offset - size, alignment, 
                types.Alignment.Down))

        self.vars[name] = local_offset

    def get_temp_context(self, backend):
        """
        Returns a context which can be used for putting temporary values on 
        the stack. When the context exits, the space used by the temporary
        variables is cleaned up.
        """
        class TemporaryContext:
            def __init__(self, start_offset):
                self.tmp_offset = start_offset
                self.total_tmp_size = 0

            def __enter__(self):
                pass

            def __exit__(self, *exc_info):
                backend._write_instr('    addi $sp, $sp, {}', self.total_tmp_size)

            def add_temp(self, size, alignment):
                """
                Makes space for a new temporary, returning the $fp offset at 
                which to write it.
                """
                old_tmp_offset = self.tmp_offset
                self.tmp_offset = (
                    types.align_address(self.tmp_offset - size, alignment,
                        types.Alignment.Down))

                size_used = old_tmp_offset - self.tmp_offset
                self.total_tmp_size += size_used
                backend._write_instr('    addi $sp, $sp, -{}', size_used)

                return self.tmp_offset

        return TemporaryContext(self.local_offset)

    def locals_size(self):
        """
        Gets the size used by all the locals.
        """
        return abs(self.local_offset) - 8

    def __getitem__(self, name):
        """
        Gets the offset to the variable on the stack, or a Register (if the
        name was bound to one of the first four parameters)
        """
        return self.vars[name]
