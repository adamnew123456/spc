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
    'Evaluates (platform? STRING)'
    if len(args) != 1:
        raise CompilerError.from_token(name_token,
            'Platform test must be of the form (platform? STRING)')

    if not lexer.is_string(args[0]):
        raise CompilerError.from_token(name_token,
            'Platform test must be of the form (platform? STRING)')

    return args[0].content.decode('ascii') == context.backend.platform_name

def evaluate(context, expression):
    """
    Evaluates a static conditional, which can be of the form:

      <INTEGER> (True when <INTEGER> != 0)
      <STRING> (True)
      (<FUNC> <ARGS...>)

    Note that this doesn't handle nested function calls.
    """
    functions = {
        'platform?': func_platform
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