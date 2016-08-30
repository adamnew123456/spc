"""
Main compiler driver - this is responsible for reading in lists and
passing information off to the backend.
"""
from collections import OrderedDict
import logging

from .errors import CompilerError
from . import expressions
from . import lexer
from . import static_expressions
from . import symbols
from . import types

LOGGER = logging.getLogger('spc.driver')

class Driver:
    """
    Responsible for reading nested lists from the lexer and passing them off
    to the compiler. The only thing it does is verify the basics of the syntax -
    the rest is left to the backend.

    This works a bit like SAX, in that the program is treated as a stream of
    primitive syntax elements rather than as an entire syntax tree. The backend
    is then like a SAX handler - it has methods for processing parts of the syntax.
    """
    def __init__(self, lex, backend):
        self.filename = lex.filename
        self.tokens = lexer.to_list(lex.lex())
        self.backend = backend

        self.static_context = static_expressions.StaticContext(self, self.backend)

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
            if not lexer.is_identifier(chunk[0]):
                raise CompilerError(self.filename, 0, 0,
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

    def parse_decl_type(self, chunk, def_name):
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

        if not lexer.is_identifier(chunk[0]):
            raise CompilerError.from_token(chunk[0],
                'Invalid declaration type')

        if chunk[0].content == 'ascii':
            if len(chunk) != 2:
                raise CompilerError.from_token(chunk[0],
                    'ascii must be of the form (ascii STRING)')

            string_literal = chunk[1]
            if not lexer.is_string(string_literal):
                raise CompilerError.from_token(chunk[0],
                    'ascii must be of the form (ascii STRING)')

            return types.StringLiteral(string_literal.content)
        elif chunk[0].content == 'array-of':
            if len(chunk) != 3:
                raise CompilerError.from_token(chunk[0],
                    'array-of must be of the form (array-of TYPE NUMBER)')

            array_type = self.parse_type(chunk[1])

            count = chunk[2]
            if not lexer.is_integer(count):
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
            return types.FunctionDecl(def_name, return_type, params)
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
                if not lexer.is_identifier(identifier):
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
                raise CompilerError.from_token(element[0],
                    'Each declaration must be of the form (IDENTIFIER KIND)')


            if not lexer.is_identifier(element[0]):
                raise CompilerError.from_token(declaration[0],
                    'Each declaration must start with an identifier')

            identifier = element[0].content
            if symbols.has_namespace(identifier):
                raise CompilerError.from_token(declaration[0],
                    'Declaration identifiers cannot be namespaced')

            declaration = self.parse_decl_type(element[1], identifier)

            decl_line = element[0].line
            decl_col = element[0].column
            self.backend.update_position(decl_line, decl_col)

            self.backend.handle_decl(identifier, declaration)

        self.backend.handle_decl_block_end()

    def process_namespace(self, namespace):
        """
        Parses a namespace statement.
        """
        if len(namespace) != 2:
            raise CompilerError.from_token(namespace[0],
                'namespace must take the form (namespace IDENTIFIER)')

        self.backend.update_position(namespace[0].line, namespace[0].column)

        if not lexer.is_identifier(namespace[0]):
            raise CompilerError.from_token(namespace[0],
                'namespace must take the form (namespace IDENTIFIER)')

        ns_name = namespace[1].content
        if symbols.has_namespace(ns_name):
            raise CompilerError.from_token(namespace[0],
                'Cannot have nested namespaces in namespace declaration')

        self.backend.handle_namespace(ns_name)

    def process_require(self, require):
        """
        Parses a require statement.
        """
        if len(require) != 2:
            raise CompilerError.from_token(require[0],
                'require must take the form (require STRING)')

        self.backend.update_position(require[0].line, require[0].column)

        filename = require[1]
        if not lexer.is_string(filename):
            raise CompilerError.from_token(require[0],
                'require must take the form (require STRING)')

        filename_text = filename.content.decode('ascii')
        self.backend.handle_require(filename_text)

    def process_export(self, exports):
        """
        Parses an export block.
        """
        self.backend.update_position(exports[0].line, exports[0].column)
        names = []

        for element in exports[1:]:
            if not lexer.is_identifier(element):
                raise CompilerError.from_token(element,
                    'Each export must be an identifier')

            names.append(element.content)

        self.backend.handle_exports(names)

    def parse_expression(self, expr):
        """
        Processes an expression.

            (ref EXPRESSION)
            (deref EXPRESSION)
            (cast TYPE EXPRESSION)

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
            CHAR
            IDENTIFIER
        """
        if isinstance(expr, list):
            if len(expr) == 0:
                raise CompilerError(self.filename, 0, 0,
                    'Cannot have a () in an expression')

            if lexer.is_integer(expr[0]):
                raise CompilerError.from_token(expr[0],
                    'Cannot call an integer')
            elif not lexer.is_identifier(expr[0]):
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
                    if not lexer.is_identifier(field):
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
        elif lexer.is_integer(expr):
            loc = expr.line, expr.column
            return expressions.Integer(loc, expr.content)
        elif lexer.is_char(expr):
            loc = expr.line, expr.column
            return expressions.Char(loc, expr.content)
        elif lexer.is_identifier(expr):
            loc = expr.line, expr.column
            return expressions.Variable(loc, expr.content)

    def process_block(self, block):
        "Processes (block STATEMENT+)"
        line = block[0].line
        column = block[0].column
        self.backend.update_position(line, column)
        self.backend.handle_block_start()

        for stmt in block[1:]:
            self.process_statement(stmt)

        self.backend.handle_block_end()

    def process_set(self, set_):
        "Processes (set_ ASSIGNABLE EXPRESSION)"
        if len(set_) != 3:
            raise CompilerError.from_token(set_[0],
                'Set must be of the form (set_ ASSIGNABLE EXPRESSION)')

        assignable = self.parse_expression(set_[1])
        expression = self.parse_expression(set_[2])

        line = set_[0].line
        column = set_[0].column
        self.backend.update_position(line, column)
        self.backend.handle_set(assignable, expression)

    def process_if(self, if_):
        "Processes (if EXPRESSION STATEMENT STATEMENT?)"
        if len(if_) not in (3, 4):
            raise CompilerError.from_token(if_[0],
                'If must be of the form (if EXPRESSION STATEMENT STATEMENT?)')

        condition = self.parse_expression(if_[1])

        line = if_[0].line
        column = if_[0].column
        self.backend.update_position(line, column)
        self.backend.handle_if(condition)

        self.process_statement(if_[2])
        self.backend.handle_else()

        if len(if_) == 4:
            self.process_statement(if_[3])

        self.backend.handle_if_end()

    def process_switch(self, switch):
        "Processes (switch ...)"
        else_read = False
        self.backend.handle_switch_start()
        for case in switch[1:]:
            if else_read:
                raise CompilerError.from_token(switch[0],
                    'Cannot have an additional case after an else')

            if not isinstance(case, list):
                raise CompilerError.from_token(switch[0],
                    'Case must be of the form (case EXPRESSION STATEMENT) or (else STATEMENT)')

            if len(case) not in (2, 3):
                raise CompilerError.from_token(switch[0],
                    'Case must be of the form (case EXPRESSION STATEMENT) or (else STATEMENT')

            if not lexer.is_identifier(case[0]):
                raise CompilerError.from_token(switch[0],
                    'Case must be of the form (case EXPRESSION STATEMENT) or (else STATEMENT)')

            if case[0].content == 'case':
                if len(case) != 3:
                    raise CompilerError.from_token(switch[0],
                        'Case must be of the form (case EXPRESSION STATEMENT)')

                cond = self.parse_expression(case[1])
                self.backend.handle_case_start(cond)

                body = self.process_statement(case[2])
                self.backend.handle_case_end()
            elif case[0].content == 'else':
                if len(case) != 2:
                    raise CompilerError.from_token(switch[0],
                        'Else must be of the form (else STATEMENT)')
                                
                else_read = True
                self.backend.handle_case_start(None)
                body = self.process_statement(case[1])
                
                self.backend.handle_case_end() 

        self.backend.handle_switch_end()

    def process_while(self, while_):
        "Processes (while EXPRESSION STATEMENT)"
        if len(while_) != 3:
            raise CompilerError.from_token(while_[0],
                'While must be of the form (while EXPRESSION STATEMENT)')

        condition = self.parse_expression(while_[1])

        line = while_[0].line
        column = while_[0].column
        self.backend.update_position(line, column)
        self.backend.handle_while(condition)

        self.process_statement(while_[2])

        self.backend.handle_while_end()

    def process_break(self, break_):
        "Processes (break)"
        if len(break_) != 1:
            raise CompilerError.from_token(break_[0],
                'Break must be of the form (break)')

        line = break_[0].line
        column = break_[0].column
        self.backend.update_position(line, column)
        self.backend.handle_break()

    def process_continue(self, continue_):
        "Process (continue)"
        if len(continue_) != 1:
            raise CompilerError.from_token(continue_[0],
                'Continue must be of the form (continue)')

        line = continue_[0].line
        column = continue_[0].column
        self.backend.update_position(line, column)
        self.backend.handle_continue()

    def process_return(self, return_):
        "Process (return EXPRESSION)"
        if len(return_) != 2:
            raise CompilerError.from_token(return_[0],
                'Return must be of the form (return EXPRESSION)')

        expr = self.parse_expression(return_[1])

        line = return_[0].line
        column = return_[0].column
        self.backend.update_position(line, column)
        self.backend.handle_return(expr)

    def process_static_if(self, if_, toplevel=False):
        """
        Process (*if EXPRESSION CODE CODE?)

        Note that toplevel should be True when a *if is invoked at the
        program's top level, and False when *if is invoked as a statement.
        """
        if len(if_) not in (3, 4):
         raise CompilerError.from_token(if_[0],
                'Static if must be of the form (*if EXPRESSION CODE CODE?)')

        result = static_expressions.evaluate(self.static_context, if_[1])
        self.backend._write_comment('===== *if =====')
        self.backend._write_comment('Condition: {}', if_[1])
        self.backend._write_comment('Result: {}', result)

        if result:
            if toplevel:
                self.process_toplevel(if_[2])
            else:
                self.process_statement(if_[2])
        elif len(if_) == 4:
            if toplevel:
                self.process_toplevel(if_[3])
            else:
                self.process_statement(if_[3])

    def process_static_error(self, error):
        "Process (*error STRING)"
        if len(error) != 2:
            raise CompilerError.from_token(error[0],
                'Static error must be of the form (*error STRING)')

        if not lexer.is_string(error[1]):
            raise CompilerError.from_token(error[0],
                'Static error must be of the form (*error STRING)')

        raise CompilerError.from_token(error[0],
                'User-generated compiler error: {}',
                error[1].content.decode('ascii'))
            
    def process_statement(self, statement):
        """
        Processes a statement.

            (block STATEMENT+)

            (set ASSIGNABLE EXPRESSION)

            (if EXPRESSION STATEMENT STATEMENT?)

            (switch
             (case EXPRESSION STATEMENT)
             (case EXPRESSION STATEMENT)
             (else STATEMENT))

            (while EXPRESSION STATEMENT)
            (break)
            (continue)

            (return EXPRESSION)

            (*if STATIC-EXPRESSION STATEMENT STATEMENT?)
            (*error STRING)

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

            elif not lexer.is_identifier(statement[0]):
                # This is actually guaranteed to be invalid, but it's easier
                # to let the expression parser take care of raising for us
                return self.parse_expression(statement)
            else:
                func = {
                    'block': self.process_block,
                    'set': self.process_set,
                    'if': self.process_if,
                    'switch': self.process_switch,
                    'while': self.process_while,
                    'break': self.process_break,
                    'continue': self.process_continue,
                    'return': self.process_return,
                    '*if': self.process_static_if,
                    '*error': self.process_static_error,
                }.get(statement[0].content)

                if func:
                    func(statement)
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
        if not lexer.is_identifier(name):
            raise CompilerError.from_token(definition[0],
                'Function definition does not have a valid name')

        if symbols.has_namespace(name.content):
            raise CompilerError.from_token(definition[0],
                'Function definition cannot have a namespace')

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

    def process_assembly_definition(self, definition):
        """
        Processes a definition using inline assembly.

            (assemble IDENTIFIER
             STRING)
        """
        if len(definition) != 3:
            raise CompilerError.from_token(definition[0],
                'Inline assembly must be of the form (assemble NAME STR)')

        name = definition[1]
        if not lexer.is_identifier(name):
            raise CompilerError.from_token(definition[0],
                'Inline assembly does not have a valid name')

        assembly = definition[2]
        if not lexer.is_string(assembly):
            raise CompilerError.from_token(definition[0],
                'Inline assembly does not have valid code')

        asm_line = definition[0].line
        asm_col = definition[0].column
        self.backend.update_position(asm_line, asm_col)

        asm_text = assembly.content.decode('ascii')
        self.backend.handle_assembly(name.content, asm_text)

    def process_toplevel(self, toplevel):
        """
        Processes toplevel forms:

            (declare ...)

            (define NAME (ARGS...)
             (declare ...)
             ...)

            (assemble IDENTIFIER STRING)

            (require STRING)

            (export IDENTIFIER*)

            (*if EXPRESSION CODE CODE?)

            (*error STRING)
        """
        if isinstance(toplevel, list):
            if toplevel[0].type != lexer.IDENTIFIER:
                raise CompilerError.from_token(toplevel[0],
                    "Unexpected '{}' at the top level", toplevel[0])

            if toplevel[0].content == 'declare':
                self.process_declaration(toplevel)
            elif toplevel[0].content == 'define':
                self.process_function_definition(toplevel)
            elif toplevel[0].content == 'assemble':
                self.process_assembly_definition(toplevel)
            elif toplevel[0].content == 'namespace':
                self.process_namespace(toplevel)
            elif toplevel[0].content == 'require':
                self.process_require(toplevel)
            elif toplevel[0].content == 'export':
                self.process_export(toplevel)
            elif toplevel[0].content == '*if':
                self.process_static_if(toplevel, toplevel=True)
            elif toplevel[0].content == '*error':
                self.process_static_error(toplevel)
            else:
                raise CompilerError.from_token(toplevel[0],
                    "Unexpected '{}' at top level", toplevel[0].content)

        elif toplevel.type == lexer.INTEGER:
            raise CompilerError.from_token(toplevel,
                    'Top level form cannot be a bare integer')
        elif toplevel.type == lexer.IDENTIFIER:
            raise CompilerError.from_token(toplevel,
                    'Top level form cannot be a bare identifier')

    def compile(self):
        """
        This process the source file, handing off syntax elements off to the
        backend for processing as it parses them.
        """
        self.backend.handle_begin_program()

        for chunk in self.tokens:
            self.process_toplevel(chunk)
            
        self.backend.handle_end_program()
