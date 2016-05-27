"""
Main compiler driver - this is responsible for reading in lists and
passing information off to the backend.
"""
from collections import OrderedDict
import logging

from .errors import CompilerError
from . import expressions
from . import lexer
from . import types

LOGGER = logging.getLogger('spc.driver')

def is_identifier(token):
    """
    Returns True if the token is a single identifier token.
    """
    return isinstance(token, lexer.Token) and token.type == lexer.IDENTIFIER

def is_integer(token):
    """
    Returns True if the token is a single numeric token.
    """
    return isinstance(token, lexer.Token) and token.type == lexer.INTEGER

def is_string(token):
    """
    Returns True if the token is a single string token.
    """
    return isinstance(token, lexer.Token) and token.type == lexer.STRING

class Backend:
    """
    A skeleton backend which has all the methods used by the driver.

    Note that all identifiers (var. names, field names, etc.) are given as
    strings, not full tokens.
    """
    def update_position(self, line, col):
        """
        Gives the position of the driver, before any method is called.

        Note that this is *not* called before:

        - handle_decl_block_end
        - handle_block_end
        - handle_else
        - handle_if_end
        - handle_while_end
        - handle_raw_expression
        """

    def handle_begin_program(self):
        """
        Called before any other handler.
        """

    def handle_end_program(self):
        """
        Called after every other handler.
        """

    def handle_decl_block_start(self):
        """
        Called before processing the elements of a declaration block.
        """

    def handle_decl(self, name, decl_type):
        """
        Handles a declaration which binds the name and the given type. decl_type
        can be anything from the spc.types module - either value types or special
        declaration types.
        """

    def handle_imports(self, names):
        """
        Called to process an import block.
        """

    def handle_exports(self, names):
        """
        Called after processing an import block.
        """

    def handle_decl_block_end(self):
        """
        Called after processing the elements of a declaration block.
        """

    def handle_func_def_start(self, name, params):
        """
        Called after reading the start of a function definition, but before the
        declaration or the body.

        Name contains the function's name, params contains the function's
        parameters' names.
        """

    def handle_func_def_end(self):
        """
        Called after reading the function body.
        """

    def handle_block_start(self):
        """
        Called before handling any of the statements inside of a block.
        """

    def handle_block_end(self):
        """
        Called after handling all of the statements inside of a block.
        """

    def handle_set(self, assignable, expression):
        """
        Called after reading a (set ASSIGNABLE EXPRESSION) statement.
        """

    def handle_if(self, cond):
        """
        Called just after reading the condition for an if, but before the 'then'
        portion.
        """

    def handle_else(self):
        """
        Called after handing the 'then' portion of an if, but before the else.
        This is called whether or not there actually is an 'else' body.
        """

    def handle_if_end(self):
        """
        Called after reading an if.
        """

    def handle_while(self, cond):
        """
        Called after reading the condition of a while, but before the body.
        """

    def handle_while_end(self):
        """
        Called after reading the body of a while.
        """

    def handle_break(self):
        """
        Called after reading a break statement.
        """

    def handle_continue(self):
        """
        Called after reading a continue statement.
        """

    def handle_return(self, expr):
        """
        Called after reading a return statement.
        """

    def handle_raw_expression(self, expression):
        """
        Handles an expression that is directly under a block.
        """

class Driver:
    """
    Responsible for reading nested lists from the lexer and passing them off
    to the compiler. The only thing it does is verify the basics of the syntax -
    the rest is left to the backend.

    This works a bit like SAX, in that the program is treated as a stream of
    primitive syntax elements rather than as an entire syntax tree. The backend
    is then like a SAX handler - it has methods for processing parts of the syntax.
    """
    def __init__(self, lexed_lists, backend):
        self.tokens = lexed_lists
        self.backend = backend

    def parse_type(self, chunk):
        """
        Parses out a type, which can be of any of the given forms:

            integer
            byte
            identifier (Considered an alias)
            (pointer-to TYPE)
            (func-pointer TYPE TYPE*)
        """
        if isinstance(chunk, list):
            if not is_identifier(chunk[0]):
                raise CompilerError(0, 0,
                    'Invalid type: {}', lexer.print_list(chunk))

            if chunk[0].content == 'pointer-to':
                if len(chunk) != 2:
                    raise CompilerError.from_token(chunk[0],
                        'pointer-to must be of the form (pointer-to TYPE)')

                return types.PointerTo(self.parse_type(chunk[1]))

            elif chunk[0].content == 'func-pointer':
                if len(chunk) < 2:
                    raise CompilerError.from_token(chunk[0],
                        'function-pointer must be of the form (func-pointer TYPE TYPE*)')

                return_type = self.parse_type(chunk[1])
                params = tuple(self.parse_type(param) for param in chunk[2:])
                return types.FunctionPointer(return_type, params)
            else:
                raise CompilerError.from_token(chunk[0],
                    'Unknown composite type: {}'.format(chunk[0].content))

        elif chunk.type != lexer.IDENTIFIER:
            raise CompilerError.from_token(chunk, 'Invalid type')

        elif chunk.content == 'integer':
            return types.Integer
        elif chunk.content == 'byte':
            return types.Byte
        else:
            return types.TypeName(chunk.content)

    def parse_decl_type(self, chunk):
        """
        Parses normal types, in addition to the forms:

            (ascii STRING): Which indicates a literal string
            (array-of TYPE SIZE): Which indicates an array of the given type
            (function TYPE TYPE*): Which indicates a function declaration
            (struct (IDENTIFIER TYPE)+): Which indicates a structure definition
            (alias TYPE): Which indicates an alias to an existing type
        """
        if not isinstance(chunk, list):
            return self.parse_type(chunk)

        if not is_identifier(chunk[0]):
            raise CompilerError.from_token(chunk[0],
                'Invalid declaration type')

        if chunk[0].content == 'ascii':
            if len(chunk) != 2:
                raise CompilerError.from_token(chunk[0],
                    'ascii must be of the form (ascii STRING)')

            string_literal = chunk[1]
            if not is_string(string_literal):
                raise CompilerError.from_token(chunk[0],
                    'ascii must be of the form (ascii STRING)')

            return types.StringLiteral(string_literal.content)
        elif chunk[0].content == 'array-of':
            if len(chunk) != 3:
                raise CompilerError.from_token(chunk[0],
                    'array-of must be of the form (array-of TYPE NUMBER)')

            array_type = self.parse_type(chunk[1])

            count = chunk[2]
            if not is_integer(count):
                raise CompilerError.from_token(chunk[0],
                    'array-of must be of the form (array-of TYPE NUMBER)')

            if count.content <= 0:
                raise CompilerError.from_token(chunk[2],
                    'Array length must be positive')

            return types.ArrayOf(array_type, count.content)
        elif chunk[0].content == 'function':
            if len(chunk) < 2:
                raise CompilerError.from_token(chunk[0],
                        'function must be of the form (function TYPE TYPE*)')

            return_type = self.parse_type(chunk[1])
            params = tuple(self.parse_type(param) for param in chunk[2:])
            return types.FunctionDecl(return_type, params)
        elif chunk[0].content == 'struct':
            if len(chunk) < 2:
                raise CompilerError.from_token(chunk[1],
                    'struct must be of the form (struct (IDENTIFIER TYPE)+)')

            fields = OrderedDict()

            if not isinstance(chunk[1], list):
                raise CompilerError.from_token(chunk[1],
                    'struct requires a group of fields')

            for field_def in chunk[1:]:
                if not isinstance(field_def, list):
                    raise CompilerError.from_token(field_def,
                        'Field must be of the form (IDENTIFIER TYPE)')

                if len(field_def) != 2:
                    raise CompilerError.from_token(chunk[0],
                        'Field must be of the form (IDENTIFIER TYPE)')

                identifier = field_def[0]
                if not is_identifier(identifier):
                    raise CompilerError.from_token(chunk[0],
                        'Field must be of the form (IDENTIFIER TYPE)')

                field_type = self.parse_type(field_def[1])
                fields[identifier.content] = field_type

            if len(fields) == 0:
                raise CompilerError.from_token(chunk[0],
                    'Struct cannot be empty')

            return types.Struct(fields)
        elif chunk[0].content == 'alias':
            if len(chunk) != 2:
                raise CompilerError.from_token(chunk[0],
                    'Type alias must be of the form (alias TYPE)')

            alias_type = self.parse_type(chunk[1])
            return types.AliasDef(alias_type)
        else:
            return self.parse_type(chunk)

    def process_declaration(self, declaration):
        """
        Declarations have the syntax:

            (declare (name DECLARATION)*)

        Where DECLARATION can be one of:

            TYPE: Which indicates a variable declaration.
            (struct (IDENT TYPE)+): Which indicates a structured type definition.
            (function RETURN IDENT*): Which indicates a function declaration.
            (alias TYPE): Which indicates an alias to an existing type.
        """
        self.backend.update_position(declaration[0].line, declaration[0].column)
        self.backend.handle_decl_block_start()

        for element in declaration[1:]:
            if not isinstance(element, list):
                raise CompilerError.from_token(element,
                    'Each declaration must be of the form (IDENTIFIER KIND)')

            if len(element) != 2:
                raise CompilerError.from_token(element,
                    'Each declaration must be of the form (IDENTIFIER KIND)')


            if not is_identifier(element[0]):
                raise CompilerError.from_token(declaration[0],
                    'Each declaration must start with an identifier')

            identifier = element[0].content
            declaration = self.parse_decl_type(element[1])

            decl_line = element[0].line
            decl_col = element[0].column
            self.backend.update_position(decl_line, decl_col)

            self.backend.handle_decl(identifier, declaration)

        self.backend.handle_decl_block_end()

    def process_import(self, imports):
        """
        Parses an import block.
        """
        self.backend.update_position(imports[0].line, imports[0].column)
        names = []

        for element in imports[1:]:
            if not is_identifier(element):
                raise CompilerError.from_token(element,
                    'Each import must be an identifier')

            names.append(element.content)

        self.backend.handle_imports(names)

    def process_export(self, exports):
        """
        Parses an export block.
        """
        self.backend.update_position(exports[0].line, exports[0].column)
        names = []

        for element in exports[1:]:
            if not is_identifier(element):
                raise CompilerError.from_token(element,
                    'Each export must be an identifier')

            names.append(element.content)

        self.backend.handle_exports(names)

    def parse_expression(self, expr):
        """
        Processes an expression.

            (ref EXPRESSION)
            (deref EXPRESSION)
            (ptr-to-int EXPRESSION)
            (int-to-ptr TYPE EXPRESSION)
            (cast TYPE EXPRESSION)

            (byte-to-int EXPRESSION)
            (int-to-byte EXPRESSION)

            (array EXPRESSION EXPRESSION)
            (field EXPRESSION IDENTIFIER+)

            (+ EXPRESSION EXPRESSION)
            (- EXPRESSION EXPRESSION)
            (* EXPRESSION EXPRESSION)
            (/ EXPRESSION EXPRESSION)
            (% EXPRESSION EXPRESSION)

            (~ EXPRESSION)
            (& EXPRESSION EXPRESSION)
            (| EXPRESSION EXPRESSION)
            (^ EXPRESSION EXPRESSION)
            (<< EXPRESSION EXPRESSION)
            (>> EXPRESSION EXPRESSION)
            (>>> EXPRESSION EXPRESSION)

            (== EXPRESSION EXPRESSION)
            (!= EXPRESSION EXPRESSION)
            (< EXPRESSION EXPRESSION)
            (> EXPRESSION EXPRESSION)
            (<= EXPRESSION EXPRESSION)
            (>= EXPRESSION EXPRESSION)

            (&& EXPRESSION EXPRESSION)
            (|| EXPRESSION EXPRESSION)

            (size-of TYPE)

            (EXPRESSION EXPRESSION*)
            INTEGER
            IDENTIFIER
        """
        if isinstance(expr, list):
            if len(expr) == 0:
                raise CompilerError(0, 0,
                    'Cannot have a () in an expression')

            if is_integer(expr[0]):
                raise CompilerError.from_token(expr[0],
                    'Cannot call an integer')
            elif not is_identifier(expr[0]):
                func = self.parse_expression(expr[0])
                args = tuple(self.parse_expression(arg)
                        for arg in expr[1:])

                return expressions.Call(func.loc, func, args)
            elif expr[0].content == 'ref':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        'ref must be of the form (ref EXPR)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.Reference(loc, expr)
            elif expr[0].content == 'deref':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        'deref must be of the form (deref EXPR)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.Dereference(loc, expr)
            elif expr[0].content == 'ptr-to-int':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        'ptr-to-int must be of the form (ptr-to-int EXPR)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.PointerToInt(loc, expr)
            elif expr[0].content == 'int-to-ptr':
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        'int-to-ptr must be of the form (int-to-ptr EXPR TYPE)')

                loc = expr[0].line, expr[0].column
                ret_type = self.parse_type(expr[1])
                expr = self.parse_expression(expr[2])
                return expressions.IntToPointer(loc, ret_type, expr)
            elif expr[0].content == 'int-to-byte':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        'int-to-byte must be of the form (int-to-byte EXPR)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.IntToByte(loc, expr)
            elif expr[0].content == 'byte-to-int':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        'byte-to-int must be of the form (byte-to-int EXPR)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.ByteToInt(loc, expr)
            elif expr[0].content == 'cast':
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        'cast must be of the form (cast TYPE EXPR)')

                loc = expr[0].line, expr[0].column
                cast_type = self.parse_type(expr[1])
                expression = self.parse_expression(expr[2])
                return expressions.Cast(loc, cast_type, expression)
            elif expr[0].content == 'array':
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        'array must be of the form (array EXPR EXPR)')

                loc = expr[0].line, expr[0].column
                array = self.parse_expression(expr[1])
                index = self.parse_expression(expr[2])
                return expressions.Array(loc, array, index)
            elif expr[0].content == 'field':
                if len(expr) < 3:
                    raise CompilerError.from_token(expr[0],
                        'field must be of the form (field EXPR IDENTIFIER+)')

                struct = self.parse_expression(expr[1])

                fields = []
                for field in expr[2:]:
                    if not is_identifier(field):
                        raise CompilerError.from_token(expr[0],
                            'field must be of the form (field EXPR IDENTIFIER+)')

                    fields.append(field.content)

                loc = expr[0].line, expr[0].column
                return expressions.Field(loc, struct, tuple(fields))
            elif expr[0].content in ('+', '-', '*', '/', '%'):
                operator = expr[0].content
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        '{op} must be of the form ({op} EXPR EXPR)'.format(op=operator))

                loc = expr[0].line, expr[0].column
                lhs = self.parse_expression(expr[1])
                rhs = self.parse_expression(expr[2])
                kind = {
                    '+': expressions.ARITH_PLUS,
                    '-': expressions.ARITH_MINUS,
                    '*': expressions.ARITH_TIMES,
                    '/': expressions.ARITH_DIVIDE,
                    '%': expressions.ARITH_MOD,
                }[operator]
                return expressions.Arithmetic(loc, kind, lhs, rhs)
            elif expr[0].content in ('&', '|', '^', '<<', '>>', '>>>'):
                operator = expr[0].content
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        '{op} must be of the form ({op} EXPR EXPR)'.format(op=operator))

                loc = expr[0].line, expr[0].column
                lhs = self.parse_expression(expr[1])
                rhs = self.parse_expression(expr[2])
                kind = {
                    '&': expressions.BitAnd,
                    '|': expressions.BitOr,
                    '^': expressions.BitXor,
                    '<<': expressions.BitShiftLeft,
                    '>>': lambda lhs, rhs: expressions.BitShiftRight(lhs, rhs, False),
                    '>>>': lambda lhs, rhs: expressions.BitShiftRight(lhs, rhs, True)
                }[operator]
                return kind(loc, lhs, rhs)
            elif expr[0].content == '~':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        '~ must be of the form (~ EXPRESSION)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.BitNot(loc, expr)
            elif expr[0].content in ('==', '!=', '<', '>', '<=', '>='):
                operator = expr[0].content
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        '{op} must be of the form ({op} EXPR EXPR)'.format(op=operator))

                loc = expr[0].line, expr[0].column
                lhs = self.parse_expression(expr[1])
                rhs = self.parse_expression(expr[2])
                kind = {
                    '==': expressions.CMP_EQ,
                    '!=': expressions.CMP_NOTEQ,
                    '<': expressions.CMP_LESS,
                    '>': expressions.CMP_GREATER,
                    '<=': expressions.CMP_LESSEQ,
                    '>=': expressions.CMP_GREATEQ,
                }[operator]
                return expressions.Compare(loc, kind, lhs, rhs)
            elif expr[0].content in ('&&', '||'):
                operator = expr[0].content
                if len(expr) != 3:
                    raise CompilerError.from_token(expr[0],
                        '{op} must be of the form ({op} EXPR EXPR)'.format(op=operator))

                loc = expr[0].line, expr[0].column
                lhs = self.parse_expression(expr[1])
                rhs = self.parse_expression(expr[2])
                kind = {
                    '&&': expressions.And,
                    '||': expressions.Or,
                }[operator]
                return kind(loc, lhs, rhs)
            elif expr[0].content == '!':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        '! must be of the form (! EXPRESSION)')

                loc = expr[0].line, expr[0].column
                expr = self.parse_expression(expr[1])
                return expressions.Not(loc, expr)
            elif expr[0].content == 'size-of':
                if len(expr) != 2:
                    raise CompilerError.from_token(expr[0],
                        'size-of must be of the form (size-of TYPE)')

                loc = expr[0].line, expr[0].column
                type_size = self.parse_type(expr[1])
                return expressions.SizeOf(loc, type_size)
            else:
                func = self.parse_expression(expr[0])
                args = tuple(self.parse_expression(arg)
                        for arg in expr[1:])

                loc = func.loc
                return expressions.Call(loc, func, args)
        elif is_integer(expr):
            loc = expr.line, expr.column
            return expressions.Integer(loc, expr.content)
        elif is_identifier(expr):
            loc = expr.line, expr.column
            return expressions.Variable(loc, expr.content)

    def process_statement(self, statement):
        """
        Processes a statement.

            (block STATEMENT+)

            (set ASSIGNABLE EXPRESSION)

            (if EXPRESSION STATEMENT STATEMENT?)

            (while EXPRESSION STATEMENT)
            (break)
            (continue)

            (return EXPRESSION)

            EXPRESSION
        """
        if isinstance(statement, list):
            if isinstance(statement[0], list):
                # No statement is of the form ((a b) c d)
                expr = self.parse_expression(statement)

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_raw_expression(expr)

            elif not is_identifier(statement[0]):
                # This is actually guaranteed to be invalid, but it's easier
                # to let the expression parser take care of raising for us
                return self.parse_expression(statement)

            elif statement[0].content == 'block':
                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_block_start()

                for sub_stmt in statement[1:]:
                    self.process_statement(sub_stmt)

                self.backend.handle_block_end()

            elif statement[0].content == 'set':
                if len(statement) != 3:
                    raise CompilerError.from_token(statement[0],
                        'Set must be of the form (set ASSIGNABLE EXPRESSION)')

                assignable = self.parse_expression(statement[1])
                expression = self.parse_expression(statement[2])

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_set(assignable, expression)

            elif statement[0].content == 'if':
                if len(statement) not in (3, 4):
                    raise CompilerError.from_token(statement[0],
                        'If must be of the form (if EXPRESSION STATEMENT STATEMENT?)')

                condition = self.parse_expression(statement[1])

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_if(condition)

                self.process_statement(statement[2])
                self.backend.handle_else()

                if len(statement) == 4:
                    self.process_statement(statement[3])

                self.backend.handle_if_end()

            elif statement[0].content == 'while':
                if len(statement) != 3:
                    raise CompilerError.from_token(statement[0],
                        'While must be of the form (while EXPRESSION STATEMENT)')

                condition = self.parse_expression(statement[1])

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_while(condition)

                self.process_statement(statement[2])

                self.backend.handle_while_end()

            elif statement[0].content == 'break':
                if len(statement) != 1:
                    raise CompilerError.from_token(statement[0],
                        'Break must be of the form (break)')

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_break()
            elif statement[0].content == 'continue':
                if len(statement) != 1:
                    raise CompilerError.from_token(statement[0],
                        'Continue must be of the form (continue)')

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_continue()
            elif statement[0].content == 'return':
                if len(statement) != 2:
                    raise CompilerError.from_token(statement[0],
                        'Return must be of the form (return EXPRESSION)')

                expr = self.parse_expression(statement[1])

                line = statement[0].line
                column = statement[0].column
                self.backend.update_position(line, column)
                self.backend.handle_return(expr)
            else:
                expr = self.parse_expression(statement)
                self.backend.handle_raw_expression(expr)
        else:
            expr = self.parse_expression(statement)
            self.backend.handle_raw_expression(expr)

    def process_function_definition(self, definition):
        """
        Process a function definition, including both the definition and the body.

            (define IDENTIFIER (IDENTIFIER*)
             DECLARATION
             STATEMENT)
        """
        if len(definition) != 5:
            raise CompilerError.from_token(definition[0],
                'Function definition must be of the form (define NAME (PARAMS) DECLARE BODY)')

        name = definition[1]
        if isinstance(name, list) or name.type != lexer.IDENTIFIER:
            raise CompilerError.from_token(definition[0],
                'Function definition does not have a valid name')

        params = definition[2]
        if not isinstance(params, list):
            raise CompilerError.from_token(definition[0],
                'Function definition require a parameter list')

        params = [param.content for param in params]
        self.backend.handle_func_def_start(name.content, params)

        declaration = definition[3]
        if not isinstance(declaration, list):
            raise CompilerError.from_token(definition[3],
                'Function definition requires a declaration block')

        if (isinstance(declaration[0], list) or
                declaration[0].type != lexer.IDENTIFIER or
                declaration[0].content != 'declare'):
            raise CompilerError.from_token(definition[0],
                'Function definition requires a declaration block')

        self.process_declaration(declaration)

        body = definition[4]
        self.process_statement(body)

        self.backend.handle_func_def_end()

    def compile(self):
        """
        This process the source file, handing off syntax elements off to the
        backend for processing as it parses them.
        """
        self.backend.handle_begin_program()

        for chunk in self.tokens:
            if isinstance(chunk, list):
                if chunk[0].type != lexer.IDENTIFIER:
                    raise CompilerError.from_token(chunk[0],
                        "Expected 'declare' or 'define' at top level")

                if chunk[0].content == 'declare':
                    self.process_declaration(chunk)
                elif chunk[0].content == 'define':
                    self.process_function_definition(chunk)
                elif chunk[0].content == 'import':
                    self.process_import(chunk)
                elif chunk[0].content == 'export':
                    self.process_export(chunk)
                else:
                    raise CompilerError.from_token(chunk[0],
                        "Expected 'declare' or 'define' at top level")

            elif chunk.type == lexer.INTEGER:
                raise CompilerError.from_token(chunk,
                        'Top level form cannot be a bare integer')
            elif chunk.type == lexer.IDENTIFIER:
                raise CompilerError.from_token(chunk,
                        'Top level form cannot be a bare identifier')

        self.backend.handle_end_program()
