"""
All the different types that the compiler handles.
"""
from collections import namedtuple

IntegerType = namedtuple('IntegerType', [])
Integer = IntegerType()

ByteType = namedtuple('ByteType', [])
Byte = ByteType()

PointerTo = namedtuple('PointerTo', ['type'])
ArrayOf = namedtuple('ArrayOf', ['type', 'count'])
FunctionPointer = namedtuple('FunctionPointer', ['return_type', 'params'])
TypeName = namedtuple('TypeName', ['name'])

# Structure is a bit of an oddity - it can't actually be used in 'raw form'
# by the user, but is always aliased in a declare block
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
