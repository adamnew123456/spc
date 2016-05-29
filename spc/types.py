"""
All the different types that the compiler handles.
"""
from collections import namedtuple
from enum import Enum

IntegerType = namedtuple('IntegerType', ['unique'])
Integer = IntegerType('integer')

ByteType = namedtuple('ByteType', ['unique'])
Byte = ByteType('byte')

PointerTo = namedtuple('PointerTo', ['type'])
FunctionPointer = namedtuple('FunctionPointer', ['return_type', 'params'])
TypeName = namedtuple('TypeName', ['name'])

# Structure is a bit of an oddity - it can't actually be used in 'raw form'
# by the user, but is always aliased in a declare block.
#
# Also, fields is an OrderedDict, since the order of fields matters for layout,
Struct = namedtuple('Struct', ['fields'])

# This is used merely to record that a function has been declared - the
# actual reified type is FunctionPointer
FunctionDecl = namedtuple('FunctionDecl', ['return_type', 'params'])

# Like FunctionDecl - the actual reified type is a pointer, this is just
# a declaration to allocate a certain amount of space somewhere
ArrayOf = namedtuple('ArrayOf', ['type', 'count'])

# The same as (array-of byte ...), but prefilled with ASCII data
StringLiteral = namedtuple('StringLiteral', ['bytes'])

AliasDef = namedtuple('AliasDef', ['type'])

# Raw types are types which can be used as variables
RAW_TYPES = (IntegerType, ByteType, TypeName, PointerTo, FunctionPointer, ArrayOf)

def decay_if_array(type_obj):
    """
    Decays arrays types into pointers.
    """
    if isinstance(type_obj, ArrayOf):
        return PointerTo(type_obj.type)
    else:
        return type_obj

def func_decl_to_ptr(func_decl):
    """
    Converts a function declaration to a pointer.
    """
    return FunctionPointer(*func_decl)

class Alignment(Enum):
    Up = 1
    Down = 2

def align_address(addr, alignment, direction=Alignment.Up):
    """
    Returns the nearest address to the given address which is aligned to the 
    given alignment.

    >>> align_address(101, 4)
    104
    >>> align_address(101, 2)
    102
    >>> align_address(101, 4, direction=Alignment.Down)
    100
    >>> align_address(101, 2, direction=Alignment.Down)
    100
    """
    if direction not in (Alignment.Up, Alignment.Down):
        raise TypeError('Alignment direction must be Up or Down')

    if addr % alignment == 0:
        return addr

    if direction == Alignment.Up:
        return addr + alignment - (addr % alignment)
    else:
        return addr - (addr % alignment)

def can_be_global(type_obj):
    """
    Returns True if the given type object represents a value that can be shared
    across files, or False otherwise.
    """
    return isinstance(type_obj,
        (IntegerType, ByteType, PointerTo, FunctionPointer, Struct,
         FunctionDecl))

def resolve_name(name, types, MAX_DEPTH=25):
    """
    Resolves a type name into a concrete type.
    """
    # Avoid crashing due to deep type searches
    MAX_DEPTH = 25

    current_depth = 0
    while isinstance(name, TypeName):
        current_depth += 1
        name = types[name.name]

        if current_depth > MAX_DEPTH:
            raise RecursionError

    return name
