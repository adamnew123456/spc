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
