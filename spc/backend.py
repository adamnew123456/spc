"""
This is the base backend, which contains parts which should be common to most
backends.
"""
from .errors import CompilerError
from .symbols import SymbolTable
from . import types

class EmptyBackend:
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

    def handle_require(self, filename):
        """
        Called to process a require statement.
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

    def handle_assembly(self, name, code):
        """
        Called after reading an inline assembly definition.
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

    def handle_switch_start(self):
        """
        Called after reading a switch, but before any cases.
        """

    def handle_switch_end(self):
        """
        Called after reading all the cases in a switch.
        """

    def handle_case_start(self, cond):
        """
        Called after reading the condition of a case, but before the body.

        Note that 'cond' is None if this is an 'else'.
        """

    def handle_case_end(self):
        """
        Called after reading the body of a case, including an 'else'
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

class BaseBackend:
    """
    A backend which includes parts common to most backends.
    """
    # Override this with the appropriate for of comment for the backend
    comment_fmt = '# {}'

    def __init__(self, output, filename, is_library, 
                builtin_functions, builtin_types):
        self.library = is_library
        self.filename = filename
        self.output_stream = output
        self.line = 0
        self.col = 0

        self.def_vals = SymbolTable(builtin_functions, is_global=True)
        self.def_types = SymbolTable(builtin_types, is_global=True)

        self.if_labels = []
        self.while_labels = []
        self.switch_labels = []

    def error(self, line, col, fmt, *args, **kwargs):
        """
        Raises a CompilerError at the given location.
        """
        raise CompilerError(self.filename, line, col,
            fmt, *args, **kwargs)

    def _type_alignment(self, type_obj):
        """
        Returns the alignment required of the given type, as an integer.
        For example, on most 32-bit platforms, 32-bit integers are word
        aligned:

        >>> self._type_alignment(types.Integer)
        4
        """
        raise NotImplementedError

    def _type_size(self, type_obj):
        """
        Returns the size of the given type in memory.

        This should include all data and *internal* alignment padding.
        """
        raise NotImplementedError

    def _resolve_if_type_name(self, type_obj):
        """
        Resolves a type name into a type object. This is useful for resolving
        aliases and structure types, both of which are stored as type
        names rather than their 'actual' types.
        """
        raise NotImplementedError

    def _field_offset(self, struct_type, field_name):
        """
        Computes the offset of a specific field in a structure type.

        This includes all internal padding used for alignment.
        """
        if field_name not in struct_type.fields:
            raise KeyError('Structure {} does not have field "{}"'.format(struct_type, field_name))

        offset = 0
        for field in struct_type.fields:
            if field_name == field:
                break

            field_type = self._resolve_if_type_name(struct_type.fields[field])
            field_alignment = self._type_alignment(field_type)

            offset = types.align_address(offset, field_alignment)
            offset += self._type_size(field_type)

        return offset

    def _array_offset(self, array_type, index):
        """
        Computes the offset of the given index, from the first element.

        This includes all internal padding used for alignment.
        """
        base_size = self._type_size(array_type.type)
        alignment = self._type_alignment(array_type.type)
        aligned_size = types.align_address(base_size, alignment)
        return aligned_size * index

    def _check_valid_types(self, type_objs):
        """
        Ensures that all the types given are well-defined - i.e. that all their
        constituent parts reference other existing types.

        Note that this does not uncover any other nastiness, like recursive
        structure types.
        """
        to_check = list(type_objs)
        checked = list()

        def check(type_obj):
            "Schedules the type to be checked if it hasn't been yet"
            if type_obj not in checked and type_obj not in to_check:
                to_check.append(type_obj)

        while to_check:
            type_obj = to_check.pop()
            checked.append(type_obj)

            type_obj = self._resolve_if_type_name(type_obj)

            if isinstance(type_obj, (types.PointerTo, types.ArrayOf)):
                check(type_obj.type)
            elif isinstance(type_obj, types.Struct):
                self._write_comment('== Structure Layout ==')
                self._write_comment('  Type: {}', type_obj)

                for field_name, field_type in type_obj.fields.items():
                    offset = self._field_offset(type_obj, field_name)
                    self._write_comment('  Field "{}" Offset: {}', field_name, offset)

                    check(field_type)
            elif isinstance(type_obj, (types.FunctionDecl, types.FunctionPointer)):
                check(type_obj.return_type)

                return_type = self._resolve_if_type_name(type_obj.return_type)
                if isinstance(return_type, types.Struct):
                    # There's no situation where you couldn't rewrite:
                    #
                    #    (function structure-tupe ...)
                    #
                    # As:
                    #
                    #    (function byte (pointer-to structure-type) ...)
                    #
                    # And simply have the caller provide a structure to 
                    # pre-populate. By doing this, we can avoid having to deal
                    # with the obvious question "where do we put this thing?"
                    self.error(self.line, self.col,
                        'Cannot return struct from function')

                for param in type_obj.params:
                    check(type_obj.params)

            # Anything not in those categories is a value type, and is valid
            # because it doesn't reference any other types

    def _write_instr(self, fmt, *args, **kwargs):
        """
        Writes an instruction to the output stream.
        """
        print(fmt.format(*args, **kwargs), file=self.output_stream)

    def _write_comment(self, fmt, *args, **kwargs):
        """
        Writes a code comment to the output stream.
        """
        formatted_comment = fmt.format(*args, **kwargs)
        print(self.comment_fmt.format(formatted_comment),
                file=self.output_stream)

    def update_position(self, line, col):
        """
        Called to update the current position in the program file.
        """
        self.line = line
        self.col = col

