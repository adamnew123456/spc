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

def pattern_match(form, token):
    """
    This provides a quick way to extract the contents of a token list,
    and see if it matches an expected pattern. Variable elements are bound to
    names in the returned object, to allow for easy retrieval.

    If the token stream matches:

    >>> pattern_match(['pointer-to', '?type'], tokens)
    {'type': ...}

    If the token stream does not match:

    >>> pattern_match(['pointer-to', '?type'], tokens)
    <Raise ValueError>

    The possible pattern forms are:

    - [...] - this indidcates a list of patterns
    - 'literal' - this indicates an identifier called 'literal'
    - '~name' - this iddicataes an identifier; the identifier is bound to the
      key 'name'
    - '$str' - this indicates a string; the string is bound to the key 'str'
    - '%int' - this indicates an integer; the integer is bound to the key 'int'
    - '#char' - this indicates a byte; the byte is bound to the key 'char'
    - '[lst' - this indicates a sublist; the sublist is bound to the key 'lst'
    - '?tok' - this indicates any token; the token is bound to the key 'tok'

    Note that any modifier may be prefixed with a '*' to get a list of things of
    that type. This is only supported as the final element.

    You can escape any modifier by placing a backslash in front of it, which
    forces the value to be a literal.
    """
    bindings = {}

    if isinstance(form, list) and isinstance(token, list):
        try:
            forms_idx = 0
            tokens_idx = 0
            while forms_idx < len(form):
                this_form = form[forms_idx]

                if isinstance(this_form, str) and this_form[0] == '*':
                    rest = []
                    this_form = this_form[1:]

                    # In order to fully bind everything, we have to know what
                    # to pull out of the sub-bindings - there might not be any
                    # if it's only literals
                    if this_form[0] in '~$%#[?':
                        bound_name = this_form[1:]
                    else:
                        bound_name = None

                    while tokens_idx < len(token):
                        this_token = token[tokens_idx]
                        sub_bindings = pattern_match(this_form, this_token)

                        if bound_name is not None:
                            rest.append(sub_bindings[bound_name])

                        tokens_idx += 1

                    bindings[bound_name] = rest
                else:
                    this_token = token[tokens_idx]
                    bindings.update(pattern_match(this_form, this_token))

                    tokens_idx += 1

                forms_idx += 1
        except IndexError:
            raise ValueError('Not enough tokens')

        if tokens_idx != len(token):
            raise ValueError('Extra tokens')
    elif lexer.is_identifier(token) and form == '\\' + token.content:
        pass
    elif form[0] == '~' and lexer.is_identifier(token):
        bindings[form[1:]] = token.content
    elif form[0] == '$' and lexer.is_string(token):
        bindings[form[1:]] = token.content
    elif form[0] == '%' and lexer.is_integer(token):
        bindings[form[1:]] = token.content
    elif form[0] == '#' and lexer.is_char(token):
        bindings[form[1:]] = token.content
    elif form[0] == '[' and isinstance(token, list):
        bindings[form[1:]] = token
    elif form[0] == '?':
        bindings[form[1:]] = token
    elif lexer.is_identifier(token) and form == token.content:
        pass
    else:
        raise ValueError('Cannot match')

    return bindings

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
        try:
            var_tbl = pattern_match('integer', chunk)
            return types.Integer
        except ValueError:
            pass

        try:
            var_tbl = pattern_match('byte', chunk)
            return types.Byte
        except ValueError:
            pass

        try:
            var_tbl = pattern_match('~ident', chunk)
            return types.TypeName(var_tbl['ident'])
        except ValueError:
            pass

        try:
            var_tbl = pattern_match(['pointer-to', '?type'], chunk)
            return types.PointerTo(self.parse_type(var_tbl['type']))
        except ValueError:
            pass

        try:
            var_tbl = pattern_match(['func-pointer', '?rtype', '*?ptype'], chunk)

            ret_type = self.parse_type(var_tbl['rtype'])
            par_types = tuple(self.parse_type(param) for param in var_tbl['ptype'])
            return types.FunctionPointer(ret_type, par_types)
        except ValueError:
            pass

        if isinstance(chunk, list):
            raise CompilerError.from_token(chunk[0], 'Invalid complex type {}', lexer.print_list(chunk))
        else:
            raise CompilerError.from_token(chunk, 'Invalid simple type')

    def parse_decl_type(self, chunk, def_name):
        """
        Parses normal types, in addition to the forms:

            (ascii STRING): Which indicates a literal string
            (array-of TYPE SIZE): Which indicates an array of the given type
            (function TYPE TYPE*): Which indicates a function declaration
            (struct (IDENTIFIER TYPE)+): Which indicates a structure definition
            (alias TYPE): Which indicates an alias to an existing type
        """
        try:
            var_tbl = pattern_match(['ascii', '$str'], chunk)
            return types.StringLiteral(var_tbl['str'])
        except ValueError:
            pass

        try:
            var_tbl = pattern_match(['array-of', '?type', '%size'], chunk)

            if var_tbl['size'] <= 0:
                raise CompilerError.from_token(
                    chunk[2],
                    'Array length must be positive')

            return types.ArrayOf(self.parse_type(var_tbl['type']),
                                 var_tbl['size'])
        except ValueError:
            pass

        try:
            var_tbl = pattern_match(['function', '?rtype', '*?ptype'], chunk)

            ret_type = self.parse_type(var_tbl['rtype'])
            par_types = tuple(self.parse_type(param) for param in var_tbl['ptype'])

            return types.FunctionDecl(def_name, ret_type, par_types)
        except ValueError:
            pass

        try:
            fields = OrderedDict()
            top_vars = pattern_match(['struct', '[fields'], chunk)

            for field in top_vars['fields']:
                field_vars = pattern_match(['~name', '?type'], field)

                field_type = self.parse_type(field_vars['type'])
                fields[field_vars['name']] = field_type

            return types.Struct(fields)
        except ValueError:
            pass

        try:
            var_tbl = pattern_match(['alias', '?type'], chunk)

            alias_type = self.parse_type(var_tbl['type'])
            return types.AliasDef(alias_type)
        except ValueError:
            pass

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

        try:
            decl_def = pattern_match(['declare', '*?decls'], declaration)
        except ValueError:
            raise CompilerError.from_token(
                declaration[0],
                'Declaration must be of the form (declare DECLARATIONS)')

        for element in decl_def['decls']:
            try:
                var_def = pattern_match(['~name', '?decl'], element)

                identifier = var_def['name']
                if symbols.has_namespace(identifier):
                    raise CompilerError.from_token(
                        declaration[0],
                        'Declaration identifiers cannot be namespaced')

                declaration = self.parse_decl_type(var_def['decl'], identifier)

                decl_line = element[0].line
                decl_col = element[0].column
                self.backend.update_position(decl_line, decl_col)

                self.backend.handle_decl(identifier, declaration)
            except ValueError:
                raise CompilerError.from_token(
                    declaration[0],
                    'Declaration must be of the form (IDENTIFIER TYPE)')

        self.backend.handle_decl_block_end()

    def process_namespace(self, namespace):
        """
        Parses a namespace statement.
        """
        self.backend.update_position(namespace[0].line, namespace[0].column)

        try:
            var_def = pattern_match(['namespace', '~ident'], namespace)

            if symbols.has_namespace(var_def['ident']):
                raise CompilerError.from_token(
                    namespace[0],
                    'Cannot have namespace in namespace declaration')

            self.backend.handle_namespace(var_def['ident'])
        except ValueError:
            raise CompilerError.from_token(
                namespace[0],
                'namespace must take the form (namespace IDENTIFIER)')

    def process_require(self, require):
        """
        Parses a require statement.
        """
        self.backend.update_position(require[0].line, require[0].column)

        try:
            var_def = pattern_match(['require', '$str'], require)

            self.backend.handle_require(var_def['str'].decode('ascii'))
        except ValueError:
            raise CompilerError.from_token(
                require[0],
                'require must take the form (require STRING)')

    def process_export(self, exports):
        """
        Parses an export block.
        """
        self.backend.update_position(exports[0].line, exports[0].column)

        try:
            var_def = pattern_match(['export', '*~idents'], exports)
            self.backend.handle_exports(var_def['idents'])
        except ValueError:
            raise CompilerError.from_token(
                exports[0],
                'export must be of the form (export IDENTIFIER...)')

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
        try:
            var_def = pattern_match(['ref', '?expr'], expr)

            loc = expr[0].line, expr[0].column
            ref_expr = self.parse_expression(var_def['expr'])
            return expressions.Reference(loc, ref_expr)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['deref', '?expr'], expr)

            loc = expr[0].line, expr[0].column
            deref_expr = self.parse_expression(var_def['expr'])
            return expressions.Dereference(loc, deref_expr)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['cast', '?type', '?expr'], expr)

            loc = expr[0].line, expr[0].column
            cast_type = self.parse_type(var_def['type'])
            cast_expr = self.parse_expression(var_def['expr'])
            return expressions.Cast(loc, cast_type, cast_expr)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['array', '?array', '?index'], expr)

            loc = expr[0].line, expr[0].column
            array = self.parse_expression(var_def['array'])
            index = self.parse_expression(var_def['index'])
            return expressions.Array(loc, array, index)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['field', '?expr', '*~fields'], expr)

            struct = self.parse_expression(var_def['expr'])

            loc = expr[0].line, expr[0].column
            return expressions.Field(loc, struct, tuple(var_def['fields']))
        except ValueError:
            pass

        try:
            var_def = pattern_match(['~op', '?lhs', '?rhs'], expr)
            if var_def['op'] not in ('+', '-', '*', '/', '%'):
                raise ValueError

            loc = expr[0].line, expr[0].column
            lhs = self.parse_expression(var_def['lhs'])
            rhs = self.parse_expression(var_def['rhs'])
            kind = {
                '+': expressions.ARITH_PLUS,
                '-': expressions.ARITH_MINUS,
                '*': expressions.ARITH_TIMES,
                '/': expressions.ARITH_DIVIDE,
                '%': expressions.ARITH_MOD,
            }[var_def['op']]

            return expressions.Arithmetic(loc, kind, lhs, rhs)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['~op', '?lhs', '?rhs'], expr)
            if var_def['op'] not in ('&', '|', '^', '<<', '>>', '>>>'):
                raise ValueError

            loc = expr[0].line, expr[0].column
            lhs = self.parse_expression(var_def['lhs'])
            rhs = self.parse_expression(var_def['rhs'])
            kind = {
                '&': expressions.BitAnd,
                '|': expressions.BitOr,
                '^': expressions.BitXor,
                '<<': expressions.BitShiftLeft,
                '>>': lambda lhs, rhs: expressions.BitShiftRight(lhs, rhs, False),
                '>>>': lambda lhs, rhs: expressions.BitShiftRight(lhs, rhs, True)
            }[var_def['op']]

            return kind(loc, lhs, rhs)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['\\~', '?arg'], expr)

            loc = expr[0].line, expr[0].column
            arg = self.parse_expression(var_def['arg'])
            return expressions.BitNot(loc, arg)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['~op', '?lhs', '?rhs'], expr)
            if var_def['op'] not in ('==', '!=', '<', '>', '<=', '>='):
                raise ValueError

            loc = expr[0].line, expr[0].column
            lhs = self.parse_expression(var_def['lhs'])
            rhs = self.parse_expression(var_def['rhs'])
            kind = {
                '==': expressions.CMP_EQ,
                '!=': expressions.CMP_NOTEQ,
                '<': expressions.CMP_LESS,
                '>': expressions.CMP_GREATER,
                '<=': expressions.CMP_LESSEQ,
                '>=': expressions.CMP_GREATEQ,
            }[var_def['op']]

            return expressions.Compare(loc, kind, lhs, rhs)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['~op', '?lhs', '?rhs'], expr)
            if var_def['op'] not in ('&&', '||'):
                raise ValueError

            loc = expr[0].line, expr[0].column
            lhs = self.parse_expression(var_def['lhs'])
            rhs = self.parse_expression(var_def['rhs'])
            kind = {
                '&&': expressions.And,
                '||': expressions.Or,
            }[var_def['op']]

            return kind(loc, lhs, rhs)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['!', '?arg'], expr)

            loc = expr[0].line, expr[0].column
            arg = self.parse_expression(var_def['arg'])
            return expressions.Not(loc, arg)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['size-of', '?type'], expr)

            loc = expr[0].line, expr[0].column
            type_size = self.parse_type(var_def['type'])
            return expressions.SizeOf(loc, type_size)
        except ValueError:
            pass

        try:
            var_def = pattern_match(['?func', '*?args'], expr)
            func = self.parse_expression(expr[0])
            args = tuple(self.parse_expression(arg)
                         for arg in var_def['args'])

            return expressions.Call(func.loc, func, args)
        except ValueError:
            pass

        try:
            var_def = pattern_match('%int', expr)
            loc = expr.line, expr.column
            return expressions.Integer(loc, var_def['int'])
        except ValueError:
            pass

        try:
            var_def = pattern_match('#char', expr)
            loc = expr.line, expr.column
            return expressions.Char(loc, var_def['char'])
        except ValueError:
            pass

        try:
            var_def = pattern_match('~ident', expr)
            loc = expr.line, expr.column
            return expressions.Variable(loc, var_def['ident'])
        except ValueError:
            pass

        if expr == []:
            raise CompilerError(self.filename, -1, -1, '() is not an expression')
        elif isinstance(expr, list):
            raise CompilerError.from_token(
                expr[0],
                'Not an expression: {}', lexer.print_list(expr))
        else:
            raise CompilerError.from_token(expr, 'Not an expression')

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
                'Set must be of the form (set ASSIGNABLE EXPRESSION)')

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

            read = False
            try:
                var_def = pattern_match(['case', '?cond', '?stmt'], case)
                read = True

                cond = self.parse_expression(var_def['cond'])
                self.backend.handle_case_start(cond)

                body = self.process_statement(var_def['stmt'])
                self.backend.handle_case_end()
            except ValueError:
                pass

            try:
                var_def = pattern_match(['else', '?stmt'], case)
                read = True
                else_read = True

                self.backend.handle_case_start(None)
                body = self.process_statement(var_def['stmt'])

                self.backend.handle_case_end()
            except ValueError:
                pass

            if not read:
                raise CompilerError.from_token(switch[0],
                    'Case must be of the form (case EXPRESSION STATEMENT) or (else STATEMENT)')

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
                self.parse_expression(statement)
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
        try:
            def_line = definition[0].line
            def_col = definition[0].column
            self.backend.update_position(def_line, def_col)

            var_def = pattern_match(
                ['define', '~name', ['*~params'], '?decl', '?body'],
                definition)

            self.backend.handle_func_def_start(var_def['name'], var_def['params'])

            # Ensure that we got a declaration that actually looks like a
            # declare block. This is done separately so that we can refer to the
            # whole decl block (via var_def['decl']) which is what handle_declaration
            # expects
            pattern_match(['declare', '*?decls'], var_def['decl'])

            self.process_declaration(var_def['decl'])
            self.process_statement(var_def['body'])
            self.backend.handle_func_def_end()
        except ValueError:
            raise CompilerError.from_token(definition[0],
                'Function definition must be of the form (define NAME (PARAMS) DECLARE BODY)')

    def process_assembly_definition(self, definition):
        """
        Processes a definition using inline assembly.

            (assemble IDENTIFIER
             STRING)
        """
        try:
            asm_line = definition[0].line
            asm_col = definition[0].column
            self.backend.update_position(asm_line, asm_col)

            var_def = pattern_match(['assemble', '~name', '$code'], definition)

            asm_text = var_def['code'].decode('ascii')
            self.backend.handle_assembly(var_def['name'], asm_text)
        except ValueError:
            raise CompilerError.from_token(definition[0],
                'Inline assembly must be of the form (assemble NAME STR)')

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
