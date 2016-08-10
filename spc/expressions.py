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

Call = namedtuple('Call', ['loc', 'func', 'params'])
Variable = namedtuple('Variable', ['loc', 'name'])
Integer = namedtuple('Integer', ['loc', 'integer'])

Reference = namedtuple('Reference', ['loc', 'expr'])
Dereference = namedtuple('Dereference', ['loc', 'expr'])
Cast = namedtuple('Cast', ['loc', 'type', 'expr'])

Array = namedtuple('Array', ['loc', 'array', 'index'])
Field = namedtuple('Field', ['loc', 'struct', 'fields'])

ARITH_PLUS, ARITH_MINUS, ARITH_TIMES, ARITH_DIVIDE, ARITH_MOD = range(5)
Arithmetic = namedtuple('Artithmetic', ['loc', 'kind', 'lhs', 'rhs'])

CMP_LESS, CMP_GREATER, CMP_LESSEQ, CMP_GREATEQ, CMP_EQ, CMP_NOTEQ = range(6)
Compare = namedtuple('Compare', ['loc', 'kind', 'lhs', 'rhs'])

BitAnd = namedtuple('BitAnd', ['loc', 'lhs', 'rhs'])
BitOr = namedtuple('BitOr', ['loc', 'lhs', 'rhs'])
BitXor = namedtuple('BitXor', ['loc', 'lhs', 'rhs'])
BitNot = namedtuple('BitNot', ['loc', 'expr'])
BitShiftLeft = namedtuple('BitShiftLeft', ['loc', 'lhs', 'rhs'])
BitShiftRight = namedtuple('BitShiftRight', ['loc', 'lhs', 'rhs', 'sign_extend'])

And = namedtuple('And', ['loc', 'lhs', 'rhs'])
Or = namedtuple('Or', ['loc', 'lhs', 'rhs'])
Not = namedtuple('Not', ['loc', 'expr'])

SizeOf = namedtuple('SizeOf', ['loc', 'type'])
