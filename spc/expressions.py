"""
The expression language is fully parsed out before passing it to a backend. This
prevents the backend from having to keep excess context in order to understand
the expression. For example, take the following expression:

    (+ a (* b (func 1 2 3)))

Compare the nested form:

    Add(a, Multiply(b, Call(func, [1, 2, 3])))

With the serial form:
    
    Enter_Add()
    Param(a)
    Enter_Multiply()
    Param(b)
    Enter_Call(func)
    Param(1)
    Param(2)
    Param(3)
    End_Call(func)
    End_Multiply()
    End_Add()
"""
from collections import namedtuple

Call = namedtuple('Call', ['func', 'params'])
Variable = namedtuple('Variable', ['name'])
Integer = namedtuple('Integer', ['integer'])

Reference = namedtuple('Reference', ['expr'])
Dereference = namedtuple('Dereference', ['expr'])
PointerToInt = namedtuple('PointerToInt', ['expr'])
IntToPointer = namedtuple('IntToPointer', ['type', 'expr'])
Cast = namedtuple('Cast', ['type', 'expr'])

Array = namedtuple('Array', ['array', 'index'])
Field = namedtuple('Field', ['struct', 'fields'])

ARITH_PLUS, ARITH_MINUS, ARITH_TIMES, ARITH_DIVIDE, ARITH_MOD = range(5)
Arithmetic = namedtuple('Artithmetic', ['kind', 'lhs', 'rhs'])

CMP_LESS, CMP_GREATER, CMP_LESSEQ, CMP_GREATEQ, CMP_EQ, CMP_NOTEQ = range(6)
Compare = namedtuple('Compare', ['kind', 'lhs', 'rhs'])

BitAnd = namedtuple('BitAnd', ['lhs', 'rhs'])
BitOr = namedtuple('BitOr', ['lhs', 'rhs'])
BitXor = namedtuple('BitXor', ['lhs', 'rhs'])
BitNot = namedtuple('BitNot', ['expr'])
BitShiftLeft = namedtuple('BitShiftLeft', ['lhs', 'rhs'])
BitShiftRight = namedtuple('BitShiftRight', ['lhs', 'rhs', 'sign_extend'])

And = namedtuple('And', ['lhs', 'rhs'])
Or = namedtuple('Or', ['lhs', 'rhs'])

SizeOf = namedtuple('SizeOf', ['type'])
