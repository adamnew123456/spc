"""
Utility functions and classes shared by multiple backends
"""
from collections import namedtuple
import logging

from .symbols import SymbolTable
from . import types

LOGGER = logging.getLogger('spc.backend_utils')

# A context is a bundle of symbol tables for values, functions, and types.
#
# The need for array_bound comes about because arrays are treated like pointers,
# but are referenced differently - their 'value' is their address (not the value
# of their first element), and they can't actually be assigned to (since they
# are a label for a hunk of memory)
Context = namedtuple('Context', ['value_defns', 'type_defns', 'array_bound', 'func_stack'])

# While loops are identified by two labels - the start label, for re-running
# the condition, and the end label, for exiting when the condition is false
WhileLabels = namedtuple('WhileLabels', ['cond', 'exit'])

# If conditions are identified by two labels - the else label, for when
# the condition is false (to skip the then block) and the end label, for
# when the condition is true (to skip the else block)
IfLabels = namedtuple('IfLabels', ['else_body', 'end'])

# Switch conditionals are handled sort of like if conditionals:
#
#  (switch          |   
#   (case T1 B1)    |       jump-if-not T1, l1prime; ...; jump l4; l1prime:
#   (case T2 B2)    |       jump-if-not T2, l2prime; ...; jump l4; l2prime:
#   (else B3))      |       ...
#                   |   l4:
class SwitchLabels:
    """
    Switch labels are similar to conditionals:

        (switch         |
         (case T1 B1)   | jump-if-not T1, case_lbl_1; ...; jump end; case_lbl_1:
         (case T2 B2)   | jump-if-not T2, case_lbl_2; ...; jump end; case_lbl_2:
         (else B3)      | ...; end_lbl:

    Since each case is processed in order, only the current case end label and 
    the end switch label is available at any given time.
    """
    def __init__(self, end_label):
        self.end_label = end_label
        self.case_end_label = None

class CoercionContext:
    """
    This is used to wrap up all the information needed to coerce values from 
    one type to another.
    """
    def __init__(self, backend, temp_context, code_templates):
        self.backend = backend
        self.temp_context = temp_context
        self.templates = code_templates

    def copy_with_context(self, new_context):
        """
        Creates a copy of this object, but within a new temporary context.
        """
        return CoercionContext(self.backend, new_context, self.templates)

    def coerce(self, input_offset, input_type, output_type):
        """
        Coerces a value, located on the stack, from the given input type to the
        given output type. Returns the stack offset of the converted
        variable and the output type.

        Raises a TypeError if this is not possible.
        """
        if input_type == output_type:
            return input_offset, output_type
        elif (input_type, output_type) == (types.Integer, types.Byte):
            return self._coerce_int_to_byte(input_offset), output_type
        elif (input_type, output_type) == (types.Byte, types.Integer):
            return self._coerce_byte_to_int(input_offset), output_type
        else:
            raise TypeError('Cannot coerce {} -> {}'.format(input_type, output_type))

    def _coerce_int_to_byte(self, input_offset):
        """
        Coerces an integer to a byte, returning the stack offset of the
        resulting byte.
        """
        byte_size = self.backend._type_size(types.Byte)
        byte_align = self.backend._type_alignment(types.Byte)
        dest_offset = self.temp_context.add_temp(byte_size, byte_align)
        tmp_reg = self.templates.tmp_regs[0]

        self.backend._write_comment('Coercing int@{} to byte@{}', 
                input_offset, dest_offset)

        self.templates.emit_load_stack_word(tmp_reg, input_offset)
        self.templates.emit_int_to_byte(tmp_reg)
        self.templates.emit_save_stack_byte(tmp_reg, dest_offset)
        return dest_offset

    def _coerce_byte_to_int(self, input_offset):
        """
        Coerces a byte to an integer, returning the stack offset of the
        resulting integer.
        """
        int_size = self.backend._type_size(types.Integer)
        int_align = self.backend._type_alignment(types.Integer)
        dest_offset = self.temp_context.add_temp(int_size, int_align)
        tmp_reg = self.templates.tmp_regs[0]

        self.backend._write_comment('Coercing byte@{} to int@{}', 
                input_offset, dest_offset)

        self.templates.emit_load_stack_byte(tmp_reg, input_offset)
        self.templates.emit_byte_to_int(tmp_reg)
        self.templates.emit_save_stack_word(tmp_reg, dest_offset)
        return dest_offset

class FunctionStack:
    """
    Tracks where variables are on the function's stack.

    Note that this makes a number of assumptions about how things are stored:

    - All arguments are stored on the stack, in reverse order. This goes
      against the calling conventions for register rich architectures, like
      MIPS, but there are enough corner cases (like copying structs by value)
      that ignoring the calling convention is worthwhile for a non-optimizing
      compiler like this.

    - Locals and temporaries are stored on the stack, in order of creation.
    """
    def __init__(self, backend):
        self.backend = backend
        self.local_offset = self._starting_locals_offset()
        self.param_offset = self._starting_param_offset()
        self.vars = {}

    def _starting_locals_offset(self):
        """
        Returns the starting offset of the local variables on the stack.
        """
        raise NotImplementedError

    def _starting_param_offset(self):
        """
        Returns the starting offset of the parameter on the stack.
        """
        raise NotImplementedError

    def _expand_stack(self, size):
        """
        Emits code to expand the stack frame by the given size.
        """
        raise NotImplementedError

    def _shrink_stack(self, size):
        """
        Emits code to reduce the stack frame by the given size.
        """
        raise NotImplementedError

    def pad_param(self, space):
        """
        Adds blank space before the next parameter.
        """
        self.param_offset += space

    def add_param(self, name, size, alignment):
        """
        Adds a new parameter to the stack.
        """
        self.param_offset = types.align_address(self.param_offset, alignment)

        self.vars[name] = self.param_offset
        self.param_offset += size

        self.backend._write_comment('Binding param "{}" to offset {}', name, self.vars[name])

    def add_local(self, name, size, alignment):
        """
        Adds a local variable to the stack.
        """
        self.local_offset = (
            types.align_address(self.local_offset - size, alignment, 
                types.Alignment.Down))

        self.vars[name] = self.local_offset
        self.backend._write_comment('Binding local "{}" to offset {}', name, self.vars[name])

    def get_temp_context(self, backend):
        """
        Returns a context which can be used for putting temporary values on 
        the stack. When the context exits, the space used by the temporary
        variables is cleaned up.
        """
        root = self

        class TemporaryContext:
            def __init__(self, start_offset):
                self.tmp_offset = start_offset
                self.total_tmp_size = 0

            def __enter__(self):
                pass

            def __exit__(self, *exc_info):
                root._shrink_stack(self.total_tmp_size)

            def add_temp(self, size, alignment):
                """
                Makes space for a new temporary, returning the $fp offset at 
                which to write it.
                """
                old_tmp_offset = self.tmp_offset
                self.tmp_offset = (
                    types.align_address(self.tmp_offset - size, alignment,
                        types.Alignment.Down))

                size_used = old_tmp_offset - self.tmp_offset
                self.total_tmp_size += size_used
                root._expand_stack(size_used)

                return self.tmp_offset

            def get_temp_context(self):
                """
                Creates a temporary context, which starts at this temporary context.
                """
                return TemporaryContext(self.tmp_offset)

        return TemporaryContext(self.local_offset)

    def expand_locals(self):
        """
        Makes enough space for the local variables on the stack.
        """
        self._expand_stack(self.locals_size())

    def cleanup_locals(self):
        """
        Cleans up the space used by the local variables on the stack.
        """
        self._shrink_stack(self.locals_size())

    def locals_size(self):
        """
        Gets the size used by all the locals.
        """
        return abs(self.local_offset) - abs(self._starting_locals_offset())

    def __getitem__(self, name):
        """
        Gets the offset to the variable on the stack, or a Register (if the
        name was bound to one of the first four parameters)
        """
        return self.vars[name]

class ContextMixin:
    """
    Manages a stack of Contexts.

    Depends upon the user of this mixin to inherit from BaseBackend in 
    addition to this one.
    """
    def __init__(self):
        self.parent_contexts = []
        self.current_context = Context(self.def_vals, self.def_types, 
            SymbolTable(), None)

    def _value_is_defined(self, name):
        """
        Returns True if the given variable is defined in the current scope, or
        False otherwise.

        This is for the static expression processor function, var-def?
        """
        return name in self.current_context.value_defns

    def _type_is_defined(self, name):
        """
        Returns True if the given type is defined in the current scope, or
        False otherwise.

        This is for the static expression processor function, var-def?
        """
        return name in self.current_context.type_defns

    def _make_func_stack(self):
        raise NotImplementedError

    def _push_context(self):
        """
        Pushes a new binding context.
        """
        old_context = self.current_context
        self.parent_contexts.append(old_context)

        self.current_context = Context(
                SymbolTable(old_context.value_defns),
                SymbolTable(old_context.type_defns),
                SymbolTable(old_context.array_bound),
                self._make_func_stack())

    def _pop_context(self):
        """
        Loads the previous binding context.
        """
        self.current_context = self.parent_contexts.pop()

    def _resolve_if_type_name(self, name):
        """
        Resolves a type name into a concrete type.
        """
        try:
            return types.resolve_name(name, self.current_context.type_defns)
        except RecursionError:
            self.error(self.line, self.col,
                'Type aliases too deep, when resolving "{}"', name)
        except KeyError as exn:
            self.error(self.line, self.col, 
                'Invalid type "{}"', str(exn))

class ThirtyTwoMixin:
    """
    Defines some information about type sizes and alignment which 32-bit
    platforms have in common.

    Depends upon the user of this mixin to inherit from ContextMixin.
    """

    def _type_alignment(self, type_obj):
        """
        Returns alignment of the given type (1 for byte, 4 for word, etc.)
        """
        type_obj = self._resolve_if_type_name(type_obj)

        if type_obj is types.Integer:
            return 4
        elif type_obj is types.Byte:
            return 1
        elif isinstance(type_obj, (types.PointerTo, types.FunctionPointer)):
            return 4
        elif isinstance(type_obj, types.ArrayOf):
            return self._type_alignment(type_obj.type)
        elif isinstance(type_obj, types.Struct):
            # The alignment only concerns the first element of the struct - 
            # the struct's internal alignment doesn't come into play
            #
            # Also, an OrderdDict's fields are not iterable, for whatever reason
            struct_types = list(type_obj.fields.values())
            return self._type_alignment(struct_types[0])
        else:
            raise TypeError('Not a compiler type: {}'.format(type_obj))

    def _type_size(self, type_obj, depth=0):
        """
        Returns the size of a type object in bytes.
        """
        MAX_DEPTH = 100

        if depth >= MAX_DEPTH:
            self.error(self.line, self.col,
                "Type nested too deeply - potential self-referential type")

        type_obj = self._resolve_if_type_name(type_obj)

        if type_obj is types.Integer:
            return 4
        elif type_obj is types.Byte:
            return 1
        elif isinstance(type_obj, (types.PointerTo, types.FunctionPointer)):
            return 4
        elif isinstance(type_obj, types.ArrayOf):
            # To avoid wasting space on the last element, this pads all the
            # elements but the last
            base_size = self._type_size(type_obj.type, depth + 1)
            return self._array_offset(type_obj, type_obj.count - 1) + base_size
        elif isinstance(type_obj, types.Struct):
            last_field = list(type_obj.fields)[-1]
            last_field_type = type_obj.fields[last_field]

            last_field_offset = self._field_offset(type_obj, last_field)
            return last_field_offset + self._type_size(last_field_type, depth + 1)
        else:
            raise TypeError('Not a compiler type: {}'.format(type_obj))

class comment_after:
    """
    Wraps a method - after the method executes, something is written to
    the log.
    """
    def __init__(self, fmt, *args, **kwargs):
        self.fmt = fmt
        self.args = args
        self.kwargs = kwargs

    def __call__(self, func):
        def wrapper(parent, *args, **kwargs):
            x = func(parent, *args, **kwargs)
            parent._write_comment(self.fmt, *self.args, **self.kwargs)
            return x

        return wrapper
