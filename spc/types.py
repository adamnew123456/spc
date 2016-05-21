"""
All the different types that the compiler handles.
"""
from collections import namedtuple
from enum import Enum

IntegerType = namedtuple('IntegerType', [])
Integer = IntegerType()

ByteType = namedtuple('ByteType', [])
Byte = ByteType()

PointerTo = namedtuple('PointerTo', ['type'])
ArrayOf = namedtuple('ArrayOf', ['type', 'count'])
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

AliasDef = namedtuple('AliasDef', ['type'])

# Raw types are types which can be used as variables
RAW_TYPES = (types.IntegerType, types.ByteType, types.TypeName, 
        types.PointerTo, types.ArrayOf, types.FunctionPointer)

def decay_if_array(type_obj):
    """
    Decays arrays types into pointers.
    """
    if isinstance(type_obj, types.ArrayOf):
        return type_obj.PointerTo(type_obj.type)
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
