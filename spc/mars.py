"""
MARS compiler backend - responsible for taking program events emitted by the
driver and converting them into code.
"""
from collections import namedtuple
import itertools
import logging
import string

from .errors import CompilerError
from . import expressions
from . import types

LOGGER = logging.getLogger('spc.mars')

# An infinite stream of fresh labels for the backend
LABEL_MAKER = ('LM_{}'.format(value) for value in itertools.count(1))

# Note that _ is not included, to avoid creating ambiguities - _ is always
# encoded as '_95'
LABEL_CHARS = string.ascii_letters + string.digits
def mangle_labels(label):
    """
    Converts a label into a form which MARS accepts.

    This outputs labels which start with 'L_' and contain
    only alphanumerics and underscores.
    """
    chunks = []
    for char in label:
        if char not in LABEL_CHARS:
            char = '_' + str(ord(char)) + '_'
        
        chunks.append(char)

    return 'L_' + ''.join(chunks)

class SymbolTable:
    """
    A lookup table which binds variables in a nested way, where access goes
    from the current symbol table up to the parent.
    """
    def __init__(self, parent=None, is_global=False, is_builtin=False):
        self.parent = parent
        self.is_builtin = is_builtin
        self.is_global = is_global
        self.bindings = {}

    def find(self, key):
        """
        Finds the symbol table in which the given key is bound, or returns
        None if the symbol is not bound.
        """
        table = self
        while table is not None:
            if key in table.bindings:
                return table
            else:
                table = table.parent

        return None

    def __getitem__(self, key):
        table = self
        while table is not None:
            if key in table.bindings:
                return table.bindings[key]
            else:
                table = table.parent

        raise KeyError('Could not resolve symbol "{}"'.format(parent))

    def __setitem__(self, key, value):
        if key in self:
            raise KeyError('Cannot overwrite existing definition of "{}"'.format(key))

        self.bindings[key] = value

    def __contains__(sef, key):
        table = self
        while table is not None:
            if key in table.bindings:
                return True
            else:
                table = table.parent

        return False

# A context is a bundle of symbol tables for values, functions, and types
Context = namedtuple('Context', ['value_defns', 'type_defns', 'func_stack'])

BUILTIN_TYPES = SymbolTable(is_builtin=True)
BUILTIN_TYPES['string'] = types.PointerTo(types.Byte)

BUILTIN_FUNCTIONS = SymbolTable(is_builtin=True)
BUILTIN_FUNCTIONS['@print-int'] = types.FunctionDecl(types.Byte, (types.Integer,))
BUILTIN_FUNCTIONS['@print-string'] = types.FunctionDecl(types.Byte, (types.TypeName('string'),))
BUILTIN_FUNCTIONS['@read-int'] = types.FunctionDecl(types.Integer, ())
BUILTIN_FUNCTIONS['@read-string'] = types.FunctionDecl(types.Byte, (types.TypeName('string'), types.Integer))
BUILTIN_FUNCTIONS['@sbrk'] = types.FunctionDecl(types.AnyPoniter, (types.Integer,))
BUILTIN_FUNCTIONS['@exit'] = types.FunctionDecl(types.Byte, ())

class FunctionStack:
    """
    Tracks where variables are on the function's stack.
    """
    # The stack discipline in use here works like this:
    #
    #     +-------
    #     | arg 2
    #     +------- offset 4
    #     | arg 1
    #     +------- offset 0
    #     | $fp
    #     +------- offset -4
    #     | $ra
    #     +------- offset -8
    #     | local 1
    #     +-------
    #     | local 2
    #     +-------
    #
    # Note that this doesn't take into account the calling convention, because
    # this compiler pretty much ignores it. All arguments are passed on the stack,
    # in reverse order. Functions keep all their local vars on the stack, to keep
    # them safe during recursion, and parameters are no exception - taking them
    # in via $a0 - $a3 would require an extra copy from the arg. register to memory

    def __init__(self):
        self.local_offset = -8
        self.param_offset = 0

    def add_param(self, name, size, alignment):
        """
        Adds a new parameter to the stack.
        """
        self.param_offset = types.align_address(self.param_offset, alignment)

        self.vars[name] = self.param_offset
        self.param_offset += size

    def add_local(self, name, size, alignment):
        """
        Adds a local variable to the stack.
        """
        self.local_offset = (
            types.align_address(self.local_offset - size, alignment, 
                types.Alignment.Down))

        self.vars[name] = local_offset

    def get_temp_context(self, backend):
        """
        Returns a context which can be used for putting temporary values on 
        the stack. When the context exits, the space used by the temporary
        variables is cleaned up.
        """
        class TemporaryContext:
            def __init__(self, start_offset):
                self.tmp_offset = start_offset
                self.total_tmp_size = 0

            def __enter__(self):
                pass

            def __exit__(self, *exc_info):
                backend._write_instr('    addi $sp, $sp, {}', self.total_tmp_size)

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
                backend._write_instr('    addi $sp, $sp, -{}', size_used)

                return self.tmp_offset

        return TemporaryContext(self.local_offset)

    def locals_size(self):
        """
        Gets the size used by all the locals.
        """
        return abs(self.local_offset) - 8

    def __getitem__(self, name):
        """
        Gets the offset to the variable on the stack, or a Register (if the
        name was bound to one of the first four parameters)
        """
        return self.vars[name]

class MarsBackend:
    """
    Emits MIPS assembly code compatible with the MARS simulator.
    """
    def __init__(self, output):
        self.output_stream = output

        program_vals = SymbolTable(BUILTIN_FUNCTIONS, is_global=True)
        program_types = SymbolTable(BUILTIN_TYPES, is_global=True)

        self.parent_contexts = []
        self.current_context = Context(program_vals, program_types, program_funcs, None)

    def _push_context(self):
        """
        Pushes a new binding context.
        """
        old_context = self.current_context
        self.parent_contexts.append(old_context)

        self.current_context = Context(
                SymbolTable(old_context.value_defns),
                SymbolTable(old_context.type_defns),
                FunctionStack())

    def _pop_context(self):
        """
        Loads the previous binding context.
        """
        self.current_context = self.parent_contexts.pop()

    def _write_instr(self, fmt, *args, **kwargs):
        """
        Writes an instruction to the output stream.
        """
        print(fmt, *args, **kwargs, file=self.output_stream)

    def _resolve_if_type_name(self, name):
        """
        Resolves a type name into a concrete type.
        """
        # Avoid crashing due to deep type searches
        MAX_DEPTH = 25
        start_name = name

        try:
            current_depth = 0
            while isinstance(name, types.TypeName):
                current_depth += 1
                name = self.current_context.type_defns[name.name]

                if current_depth > MAX_DEPTH:
                    raise CompilerError(0, 0, 
                        'Type aliases too deep, when resolving "{}"', start_name.name)

            return name
        except KeyError:
            raise CompilerError(0, 0, 'Invalid type "{}"', name)

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
            struct_types = type_obj.fields.values()
            return self._type_alignment(next(struct_types))

    def _field_offset(self, struct_type, field_name):
        """
        Computes the offset of a specific field in the given structure.
        """
        if field_name not in struct_type:
            raise KeyError('Structure {} does not have field {}'.format(struct_type, field_name))

        offset = 0
        for field in type_obj.fields:
            if field_name == field:
                break

            field_type = self._resolve_if_type_name(type_obj.fields[field])
            offset = types.align_address(offset, field_alignment)
            offset += self._type_size(field_type, depth + 1)

        return offset

    def _array_offset(self, array_type, index):
        """
        Computes the offset of the given index, from the first element, taking
        into account the size and alignment of internal elements.
        """
        base_size = self._type_size(array_type.type)
        aligned_size = types.align_address(base_size, alignment)
        return aligned_size * index

    def _type_size(self, type_obj, depth=0):
        """
        Returns the size of a type object in bytes.
        """
        MAX_DEPTH = 100

        if depth >= MAX_DEPTH:
            raise CompilerError(0, 0, 
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
            base_size = self._type_size(type_obj.type)
            return self._array_offset(type_obj, types_obj.count - 1) + base_size
        elif isinstance(type_obj, types.Struct):
            last_field = list(type_obj.fields)[-1]
            last_field_type = type_obj.fields[last_field]

            last_field_offset = self._field_offset(type_obj, last_field)
            return last_field_offset + self._type_size(last_field_type)

    def _check_valid_types(self, type_objs):
        """
        Ensures that all the types given are well-defined - i.e. that all their
        constituent parts reference other existing types.

        Note that this does not uncover any other nastiness, like recursive
        types.
        """
        # Recursive types are uncovered later, when actually computing the size
        # of the types - obviously, computing the size of a type which includes
        # itself as a member is going to fail
        to_check = set(type_objs)
        checked = set()

        def check(type_obj):
            "Schedules the type to be checked if it hasn't been yet"
            if type_obj not in checked:
                to_check.add(type_obj)

        while to_check:
            type_obj = to_check.remove()
            checked.add(type_obj)

            type_obj = self._resolve_if_type_name(type_obj)

            if isinstance(type_obj, (types.PointerTo, types.ArrayOf)):
                check(type_obj.type)
            elif isinstance(type_obj, types.Struct):
                for field_type in type_obj.fields.values():
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
                    raise CompilerError(0, 0, 'Cannot return struct from function')

                for param in type_obj.params:
                    check(type_obj.params)

            # Anything not in those categories is a value type, and is valid
            # because it doesn't reference any other types

    def handle_begin_program(self):
        """
        Initializes the backend for outputting a new program.
        """
        self.in_function = False

        # Where to jump to, to exit the current function. Having a single exit
        # point allows the code to be smaller, by not duplicating stack handling
        # logic
        self.func_exit_label = None

        # These are basically state flags, which ensure  that the program is
        # valid by allowing us to detect if there are duplicate declaration
        # blocks (which aren't allowed by the language)
        self.read_func_decls = False
        self.read_top_decls = False

    def handle_end_program(self):
        """
        Does nothing - nothing is required after writing the final function.
        """

    def handle_decl_block_start(self):
        """
        Handles the start of a declaration block.

        For the top-level declaration block, this writes out the .data 
        section for the main program. Function-level declaration blocks
        don't do anything.
        """
        if not self.in_function:
            if self.read_top_decls:
                raise CompilerError(0, 0, 'Cannot have >1 top-level declaration blocks')

            self.read_top_decls = True

            self._write_instr('.data')
        else:
            if self.read_func_decls:
                raise CompilerError(0, 0, 'Cannot have >1 function declaration blocks')

            self.read_func_decls = True

    def handle_decl(self, name, decl_type):
        """
        Handles declarations inside of a declaration block.

        At the top level, this will either allocate space in the .data section,
        or just add information to the global context. In a function, this will
        either reserve space on the stack, or just add information to the local
        context.
        """
        was_type_name = isinstance(name, types.TypeName)
        decl_type = self._resolve_if_type_name(decl_type)

        if was_type_name or isinstance(decl_type, types.RAW_TYPES):
            self.current_context.value_defns[name] = types.decay_if_array(decl_type)

            if self.in_function:
                # Raw types have to be allocated stack space
                type_size = self._type_size(decl_type)
                alignment = self._type_alignment(decl_type)
                self.current_context.func_stack.add_local(name, type_size, alignment)
            else:
                # Raw types have to be allocated space inside of .data 
                # and given a label
                label = mangle_label(name)
                self._write_instr('{}:', label)

                size = self._type_size(decl_type)
                self._write_instr('    .space {}', size)
        elif isinstance(decl_type, types.Struct):
            # Structure types are treated as structure definitions, which 
            # bind a type definition
            self.current_context.type_defns[name] = decl_type
        elif isinstance(decl_type, types.FunctionDecl):
            # Function declarations signify a function that is defined 
            # somewhere
            if self.in_function:
                raise CompilerError(0, 0, 'Cannot declare nested functions')

            self.current_context.value_defns[name] = decl_type
        elif isinstance(decl_type, types.AliasDef):
            # Alias definitions bind an existing type to a new name
            self.current_context.type_defns[name] = decl_type.type

    def handle_decl_block_end(self):
        """
        Handles the end of a declaration block.

        At the top level, this starts the .text section and writes a jump to
        main. In a function, this adds enough space to the stack frame for
        the local variables.
        """
        # Make sure that all the types we just defined don't reference
        # any types that don't exist, or do any other nasty things
        for symbol_tbl in self.current_context:
            # (Not all things in the context are symbol tables, but most are)
            if isinstance(symbol_tbl, SymbolTable):
                self._check_valid_types(symbol_tbl.bindings.values())

        if not self.in_function:
            self._write_instr('.text')
            self._write_instr('    j main')
        else:
            self._write_instr('    addi $sp, -{}', self.current_context.func_stack.locals_size())

    def handle_func_def_start(self, name, params):
        """
        Handles the beginning of a function.
        """
        self.in_function = True
        self.func_exit_label = next(LABEL_MAKER)

        try:
            func_defn = self.current_context.func_defns[name]
        except KeyError as exn:
            raise CompilerError(0, 0, 'Undefined function "{}"', name)

        self._push_context()

        for param, param_type in zip(self.params, func_defn.params):
            param_type = self._resolve_if_type_name(param_type)

            type_size = self._type_size(param_type)
            alignment = self._type_alignment(type_size)
            self.current_context.func_stack.add_param(param, type_size, alignment)

            self.current_context.value_defns[param] = param_type

        func_label = mangle_label(name)
        self._write_instr('{}:', func_label)

        # Setup the function's stack frame before adding any other code
        self._write_instr('    sw $fp, -4($sp)')
        self._write_instr('    sw $ra, -8($sp)')
        self._write_instr('    addi $fp, $sp, -8')

        self._write_instr('    sw $fp, -4($sp)')
        self._write_instr('    sw $ra, -8($sp)')

        self._write_instr('    addi $fp, $sp, 0')
        self._write_instr('    addi $sp, $sp, -8')

    def handle_func_def_end(self):
        """
        Handles the end of the function definition.

        Writes out the end label and the code for doing a function return.
        """
        self._write_instr('{}:', self.func_exit_label)

        frame_size = self.current_context.func_stack.locals_size() + 8
        self._write_instr('    addi $sp, $sp, {}', frame_size)
        self._write_isntr('    lw $fp, -4($sp)')
        self._write_instr('    lw $ra, -8($sp)')
        self._write_instr('    jr $ra')

        self._pop_context()

        self.in_function = False
        self.func_exit_label = None

    def handle_block_start(self):
        """
        Handles the beginning of a begin block.

        Does nothing, since blocks don't have their own scope, or otherwise do
        anything useful besides group statements.
        """

    def handle_block_end(self):
        """
        Handles the end of a begin block.

        Does nothing - see handle_block_start for why
        """

    def _memcpy(self, tmp_reg, size, src_start_reg, src_start_offset, dest_start_reg, dest_start_offset):
        """
        Writes out a series of instructions which copies size bytes from the 
        start to the end, using tmp_reg as the register.
        """
        for byte in range(size):
            self._write_instr('    lb ${}, {}(${})', tmp_reg, byte + src_start_offset, src_start_reg)
            self._write_instr('    sb ${}, {}(${})', tmp_reg, byte + dest_start_offset, dest_start_reg)
