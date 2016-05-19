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
IntToPointer = namedtuple('IntToPointer', ['expr'])
Cast = namedtuple('Cast', ['type', 'expr'])
Func = namedtuple('Func', ['name'])

ArrayGet = namedtuple('ArrayGet', ['array', 'index'])
StructGet = namedtuple('StructGet', ['struct', 'fields'])

ARITH_PLUS, ARITH_MINUS, ARITH_TIMES, ARITH_DIVIDE, ARITH_MOD = range(5)
Arithmetic = namedtuple('Artithmetic', ['kind', 'lhs', 'rhs'])

CMP_LESS, CMP_GREATER, CMP_LESSEQ, CMP_GREATEQ, CMP_EQ, CMP_NOTEQ = range(6)
Compare = namedtuple('Compare', ['kind', 'lhs', 'rhs'])

SizeOf = namedtuple('SizeOf', ['type'])
