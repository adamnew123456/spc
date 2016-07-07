"""
This module is responsible for evaluating static
expressions.
"""
from .errors import CompilerError
from . import lexer

class StaticContext:
    """
    This carries around the information necessary to evaluate static
    expressions
    """
    def __init__(self, driver, backend):
        self.driver = driver
        self.backend = backend

def func_platform(context, name_token, args):
    """
    Evaluates (platform? STRING STRING)

    Returns True if the backend's OS and architecture match the given pattern,
    or False otherwise (OS is argument 1, architecture is argument 2)

    Note that either may be the wildcard '*' to match over any value.
    """
    if len(args) != 2:
        raise CompilerError.from_token(name_token,
            'Platform test must be of the form (platform? STRING STRING)')

    if not lexer.is_string(args[0]):
        raise CompilerError.from_token(name_token,
            'Platform test must be of the form (platform? STRING STRING)')

    if not lexer.is_string(args[1]):
        raise CompilerError.from_token(name_token,
            'Platform test must be of the form (platform? STRING STRING)')

    os = args[0].content.decode('ascii')
    arch = args[1].content.decode('ascii')

    real_os, real_arch = context.backend._platform()
    if os != real_os and os != '*':
        return False

    if arch != real_arch and arch != '*':
        return False

    return True

def is_var_defined(context, name_token, args):
    """
    Evaluates (var-def? IDENTIFIER)

    Returns True if the variable is defined in the current scope, False 
    otherwise.
    """
    if len(args) != 1:
        raise CompilerError.from_token(name_token,
            'Variable definition test must be of the form (var-def? STRING)')

    if not lexer.is_identifier(args[0]):
        raise CompilerError.from_token(name_token,
            'Variable definition test must be of the form (var-def? STRING)')

    name = args[0].content
    return context.backend._value_is_defined(name)

def is_type_defined(context, name_token, args):
    """
    Evaluates (type-def? IDENTIFIER)

    Returns True if the type is defined in the current scope, False 
    otherwise.
    """
    if len(args) != 1:
        raise CompilerError.from_token(name_token,
            'Variable definition test must be of the form (var-def? STRING)')

    if not lexer.is_identifier(args[0]):
        raise CompilerError.from_token(name_token,
            'Variable definition test must be of the form (var-def? STRING)')

    name = args[0].content
    return context.backend._type_is_defined(name)

def evaluate(context, expression):
    """
    Evaluates a static conditional, which can be of the form:

      <INTEGER> (True when <INTEGER> != 0)
      <STRING> (True)
      (<FUNC> <ARGS...>)

    Note that this doesn't handle nested function calls.
    """
    functions = {
        'platform?': func_platform,
        'var-def?': is_var_defined,
        'type-def?': is_type_defined,
    }

    if lexer.is_identifier(expression):
        raise CompilerError.from_token(expression,
            'Bare identifier is not valid *if expression')
    elif lexer.is_integer(expression):
        
        return expression.content != 0
    elif lexer.is_string(expression):
        return True
    else:
        if len(expression) == 0:
            raise CompilerError(context.driver.filename, -1, -1,
                '() is not valid if* conditional')

        func = expression[0]
        args = expression[1:]

        if not lexer.is_identifier(func):
            raise CompilerError(context.driver.filename, -1, -1,
                '{} is not valid if* function', func)

        try:
            func = functions[func.content]
            return func(context, func, args)
        except KeyError:
            raise CompilerError.from_token(func,
                'if* function not defined: {}', func.content)
