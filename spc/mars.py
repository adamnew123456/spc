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

BUILTIN_TYPES = SymbolTable(is_builtin=True)
BUILTIN_TYPES['string'] = types.PointerTo(types.Byte)

BUILTIN_FUNCTIONS = SymbolTable(is_builtin=True)
BUILTIN_FUNCTIONS['@print-int'] = types.FunctionDecl(types.Byte, (types.Integer,))
BUILTIN_FUNCTIONS['@print-string'] = types.FunctionDecl(types.Byte, (types.TypeName('string'),))
BUILTIN_FUNCTIONS['@read-int'] = types.FunctionDecl(types.Integer, ())
BUILTIN_FUNCTIONS['@read-string'] = types.FunctionDecl(types.Byte, (types.TypeName('string'), types.Integer))
BUILTIN_FUNCTIONS['@sbrk'] = types.FunctionDecl(types.AnyPoniter, (types.Integer,))
BUILTIN_FUNCTIONS['@exit'] = types.FunctionDecl(types.Byte, ())
