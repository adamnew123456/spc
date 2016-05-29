"""
MARS compiler backend - responsible for taking program events emitted by the
driver and converting them into code.
"""
from collections import namedtuple
import logging
import string

from ..backend import BaseBackend
from ..errors import CompilerError
from .. import expressions
from ..symbols import SymbolTable
from .. import types
from ..util import *

LOGGER = logging.getLogger('spc.mars')

LABEL_MAKER = make_label_maker()

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

BUILTIN_TYPES = SymbolTable(is_builtin=True)
BUILTIN_TYPES['string'] = types.PointerTo(types.Byte)

BUILTIN_FUNCTIONS = SymbolTable(is_builtin=True)

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
        self.vars = {}

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

        self.vars[name] = self.local_offset

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

            def get_temp_context(self):
                """
                Creates a temporary context, which starts at this temporary context.
                """
                return TemporaryContext(self.tmp_offset)

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

class MarsBackend(BaseBackend):
    """
    Emits MIPS assembly code compatible with the MARS simulator.
    """
    def __init__(self, output, is_library):
        super().__init__(output, is_library,
                BUILTIN_FUNCTIONS, BUILTIN_TYPES)

        self.undefined_funcs = set()

        self.parent_contexts = []
        self.current_context = Context(self.def_vals, self.def_types, 
            SymbolTable(), None)

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
                FunctionStack())

    def _pop_context(self):
        """
        Loads the previous binding context.
        """
        self.current_context = self.parent_contexts.pop()

    class comment_after:
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
                    raise CompilerError(self.line, self.col,
                        'Type aliases too deep, when resolving "{}"', start_name.name)

            return name
        except KeyError:
            raise CompilerError(self.line, self.col, 'Invalid type "{}"', name)

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
            raise CompilerError(self.line, self.col,
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
            return self._array_offset(type_obj, type_obj.count - 1) + base_size
        elif isinstance(type_obj, types.Struct):
            last_field = list(type_obj.fields)[-1]
            last_field_type = type_obj.fields[last_field]

            last_field_offset = self._field_offset(type_obj, last_field)
            return last_field_offset + self._type_size(last_field_type)
        else:
            raise TypeError('Not a compiler type: {}'.format(type_obj))

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
        Handles the end of the program, after the last function is defined.
        """
        if self.undefined_funcs:
            raise CompilerError(0, 0,
                'Functions not defined: {}', self.undefined_funcs)

    def handle_decl_block_start(self):
        """
        Handles the start of a declaration block.

        For the top-level declaration block, this writes out the .data 
        section for the main program. Function-level declaration blocks
        don't do anything.
        """
        if not self.in_function:
            if self.read_top_decls:
                raise CompilerError(self.line, self.col, 'Cannot have >1 top-level declaration blocks')

            self.read_top_decls = True
            self._write_instr('.data')
        else:
            if self.read_func_decls:
                raise CompilerError(self.line, self.col, 'Cannot have >1 function declaration blocks')

            self.read_func_decls = True

        self._write_comment('== Binding declarations ==')

    def handle_decl(self, name, decl_type):
        """
        Handles declarations inside of a declaration block.

        At the top level, this will either allocate space in the .data section,
        or just add information to the global context. In a function, this will
        either reserve space on the stack, or just add information to the local
        context.
        """
        was_type_name = isinstance(decl_type, types.TypeName)
        decl_type = self._resolve_if_type_name(decl_type)

        if isinstance(decl_type, types.StringLiteral):
            self._write_comment('  Declaring string {}', name)

            escaped = unescape_bytes(decl_type.bytes).encode('ascii')
            self.current_context.value_defns[name] = types.PointerTo(types.Byte)
            self.current_context.array_bound[name] = len(escaped)

            if self.in_function:
                # We can't really use .asciiz in a function, so we'll have to
                # make do with copying bytes manually
                escaped += b'\0'
                self.current_context.func_stack.add_local(name, len(escaped), 1)
                base_addr = self.current_context.func_stack.local_offset
                for idx, byte in enumerate(escaped):
                    self._write_instr('    li $t0, {}', byte)
                    self._write_instr('    sb $t0, {}($fp)', base_addr + idx)
            else:
                self._write_instr('{}:', mangle_label(name))
                self._write_instr('    .asciiz "{}"',
                    decl_type.bytes.decode('ascii'))
        elif was_type_name or isinstance(decl_type, types.RAW_TYPES):
            self._write_comment('  Declaring variable {} :: {}', name, decl_type)

            was_array = isinstance(decl_type, types.ArrayOf)
            self.current_context.value_defns[name] = types.decay_if_array(decl_type)

            if self.in_function:
                # Raw types have to be allocated stack space
                size = self._type_size(decl_type)
                alignment = self._type_alignment(decl_type)
                self.current_context.func_stack.add_local(name, size, alignment)

                self._write_comment('  At offset: {}($fp)', 
                    self.current_context.func_stack.local_offset)
            else:
                # Raw types have to be allocated space inside of .data 
                # and given a label
                self._write_comment('  As a global')

                label = mangle_label(name)
                self._write_instr('{}:', label)

                size = self._type_size(decl_type)
                self._write_instr('    .space {}', size)

            if was_array:
                self.current_context.array_bound[name] = size

        elif isinstance(decl_type, types.Struct):
            # Structure types are treated as structure definitions, which 
            # bind a type definition
            self._write_comment('  Declaring structure {} :: {}', name, decl_type)
            self.current_context.type_defns[name] = decl_type
        elif isinstance(decl_type, types.FunctionDecl):
            # Function declarations signify a function that is defined 
            # somewhere
            if self.in_function:
                raise CompilerError(self.line, self.col, 'Cannot declare nested functions')

            self.undefined_funcs.add(name)

            self._write_comment('  Declaring function {} :: {}', name, decl_type)
            self.current_context.value_defns[name] = decl_type
        elif isinstance(decl_type, types.AliasDef):
            # Alias definitions bind an existing type to a new name
            self._write_comment('  Declaring alias {} => {}', name, decl_type.type)
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

        if self.in_function:
            self._write_instr('    addi $sp, $sp, -{}', 
                self.current_context.func_stack.locals_size())
        else:
            self._write_instr('.text')

            if not self.library:
                self._write_instr('    jal {}', mangle_label('main'))
                self._write_instr('    li $v0, 10')
                self._write_instr('    syscall')

    def handle_imports(self, names):
        """
        Handles an import block.

        MARS doesn't actually require anything to import symbols into the
        current file, so this doesn't import anything. It doesn't work at
        function level, because of the risk of polluting the global namespace
        and creating clashes between 'local' imports and global imports.
        """
        if self.in_function:
            raise CompilerError(self.line, self.col,
                'Cannot import values inside of function')

        for name in names:
            try:
                type_of = self.current_context.value_defns[name]
            except KeyError:
                raise CompilerError(self.line, self.col,
                    'Undefined import "{}"', name)

            if not types.can_be_global(type_of):
                raise CompilerError(self.line, self.col,
                    'Cannot import value of type "{}"', type_of)

            self.undefined_funcs.remove(name)

    def handle_exports(self, names):
        """
        Handles an export block.
        
        This will mark a value as either .extern (if it is a value) or as 
        .globl (if it is a function).

        This is invalid at the function level, because values inner to 
        functions aren't available outside that function.
        """
        if self.in_function:
            raise CompilerError(self.line, self.col,
                'Cannot import values inside of function')

        for name in names:
            try:
                type_of = self.current_context.value_defns[name]
            except KeyError:
                raise CompilerError(self.line, self.col,
                    'Undefined export "{}"', name)

            if not types.can_be_global(type_of):
                raise CompilerError(self.line, self.col,
                    'Cannot export value of type "{}"', type_of)

            if isinstance(type_of, types.FunctionDecl):
                self._write_instr('.globl {}', mangle_label(name))
            else:
                if name in self.current_context.array_bound:
                    size = self.current_context.array_bound[name]
                else:
                    size = self._type_size(name)

                self._write_instr('.extern {}, {}', mangle_label(name), size)

    def handle_func_def_start(self, name, params):
        """
        Handles the beginning of a function.
        """
        self.in_function = True
        self.func_exit_label = next(LABEL_MAKER)

        self.undefined_funcs.remove(name)

        try:
            func_defn = self.current_context.value_defns[name]
        except KeyError as exn:
            raise CompilerError(self.line, self.col, 'Undefined function "{}"', name)

        if not isinstance(func_defn, types.FunctionDecl):
            raise CompilerError(self.line, self.col, 'Value {} is not a function', name)

        self._push_context()

        if len(params) != len(func_defn.params):
            raise CompilerError(self.line, self.col, 'Type has {} params, definition has {}',
                len(func_defn.params), len(params))

        self._write_comment('== Binding parameters ==')
        for param, param_type in zip(params, func_defn.params):
            param_type = self._resolve_if_type_name(param_type)

            type_size = self._type_size(param_type)
            alignment = self._type_alignment(param_type)
            self.current_context.func_stack.add_param(param, type_size, alignment)

            self._write_comment('  Declaring parameter {} :: {} at {}($fp)', 
                param, param_type, self.current_context.func_stack.param_offset - type_size)
            self.current_context.value_defns[param] = param_type

        self.func_ret_type = self._resolve_if_type_name(func_defn.return_type)

        func_label = mangle_label(name)
        self._write_instr('{}:', func_label)

        # Setup the function's stack frame before adding any other code
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
        self._write_instr('    lw $fp, -4($sp)')
        self._write_instr('    lw $ra, -8($sp)')
        self._write_instr('    jr $ra')

        self._pop_context()

        self.in_function = False
        self.read_func_decls = False
        self.func_exit_label = None
        self.func_ret_type = None

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
    
    def _memcpy_opt(self, tmp_reg, type_obj, src_start_reg, src_start_offset, dest_start_reg, dest_start_offset):
        """
        An optimized version of memcpy, which will do a single store word if 
        the input value is of the right type.
        """
        if isinstance(type_obj, (types.IntegerType, types.PointerTo, types.FunctionPointer)):
            self._write_instr('    lw ${}, {}(${})', tmp_reg, src_start_offset, src_start_reg)
            self._write_instr('    sw ${}, {}(${})', tmp_reg, dest_start_offset, dest_start_reg)
        else:
            self._memcpy(tmp_reg, self._type_size(type_obj),
                    src_start_reg, src_start_offset, 
                    dest_start_reg, dest_start_offset)

    def _memcpy(self, tmp_reg, size, src_start_reg, src_start_offset, dest_start_reg, dest_start_offset):
        """
        Writes out a series of instructions which copies size bytes from the 
        start to the end, using tmp_reg as the register.
        """
        for byte in range(size):
            self._write_instr('    lb ${}, {}(${})', tmp_reg, byte + src_start_offset, src_start_reg)
            self._write_instr('    sb ${}, {}(${})', tmp_reg, byte + dest_start_offset, dest_start_reg)

    @comment_after('== End ==')
    def _compile_expression(self, expr, temp_context, by_ref=False):
        """
        Unwraps an expression into a series of temporary assignments, putting
        the final result onto a position on the stack.

        Returns a tuple indicating the offset of the expression's result 
        (relative to $fp) and the type of the result.

        This can behave one of two ways:

        - By value, where the expression's value is put into the destination
        - By reference, where the expression's address is put into the destination
        """
        # Since expressions can be nested, but assembly statements cannot be,
        # expressions have to be 'unwrapped' in order to be of any use.
        #
        # For example, consider the following:
        #
        # (set x (+ (+ a b) (+ c d)))
        #
        # This would have to be done as the following three statements:
        #
        #   add $t0, a, b
        #   add $t1, c, d
        #   add x, $t0, $t1

        # by_ref is useful to have - conside the following example with structs:
        #
        # - structs which are assigned to other structs should do so by 
        #   copying one to another, bit by bit. This is where by_ref == False.
        #
        # - structs whose fields are being set should do so by loading
        #   the address of the struct, not the value (since copying would mean
        #   that the original struct isn't changed). This is where by_ref == True.
        #
        # The litmus test for whether or not something can be used in a ref 
        # context is whether it makes sense to assign to it. In this way, it is
        # similar to the lvalue/rvalue distinction in C (but a bit more liberal)
        self._write_comment('== Compiling Expression ==')
        self._write_comment('  Expression: {}', expr)
        self._write_comment('  by_ref? {}', by_ref)

        if isinstance(expr, expressions.Variable):
            # by_ref obviously makes sense - otherwise, assignment to a variable
            # couldn't work at all!

            owning_scope = self.current_context.value_defns.find(expr.name)
            if owning_scope is None:
                raise CompilerError(*expr.loc, 'No variable "{}" in scope', expr.name)

            type_of = owning_scope[expr.name]
            if isinstance(type_of, types.TypeName):
                type_of = self._resolve_if_type_name(type_of)

            if isinstance(type_of, types.FunctionDecl):
                if by_ref:
                    # You can't logically assign to the address you get back, so
                    # it doesn't make sense to even try
                    raise CompilerError(*expr.loc, 'Cannot use function in a ref context')

                type_of = types.func_decl_to_ptr(type_of)

                # Set the flag anyway, since trying to load a function 
                # 'by value' would load code, which doesn't make sense
                self._write_comment('  Variable load: function pointer {}', expr.name)
                by_ref = True

            if (isinstance(type_of, types.PointerTo) and 
                    expr.name in self.current_context.array_bound):
                # Arrays are promoted to by_ref, but for a special reason - the
                # want to be pointers (thus a non-by-ref load should get back
                # the address) but are more like values. Making them by_ref
                # allows them to be used like pointers, but with the side
                # effect that they can't be assigned to
                if by_ref:
                    raise CompilerError(*expr.loc, 'Cannot use array in a ref context')

                self._write_comment('  Variable load: array {}', expr.name)
                by_ref = True

            if by_ref:
                type_size = self._type_size(types.Integer)
                type_align = self._type_alignment(types.Integer)
            else:
                type_size = self._type_size(type_of)
                type_align = self._type_alignment(type_of)

            dest_offset = temp_context.add_temp(type_size, type_align)

            if owning_scope.is_global:
                # Global variables are somewhere in .data land, labeled by their name
                self._write_instr('    la $t0, {}', mangle_label(expr.name))
            elif owning_scope.is_builtin:
                raise CompilerError(*expr.loc, 'Builtin values cannot be used, except to be called')
            else:
                # Local variables are on the stack
                stack_offset = self.current_context.func_stack[expr.name]
                self._write_instr('    addi $t0, $fp, {}', stack_offset)

            if by_ref:
                self._write_instr('    sw $t0, {}($fp)', dest_offset)
            else:
                self._memcpy_opt('t1', type_of,
                    't0', 0, 
                    'fp', dest_offset)

            return dest_offset, type_of
        elif isinstance(expr, expressions.Integer):
            if by_ref:
                # by_ref is invalid, clearly, since you can't assign to an integer
                raise CompilerError(*expr.loc, 'Cannot use an integer literal in a ref context')

            dest_offset = temp_context.add_temp(self._type_size(types.Integer),
                                                self._type_alignment(types.Integer))

            self._write_instr('    li $t0, {}', expr.integer)
            self._write_instr('    sw $t0, {}($fp)', dest_offset)
            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Reference):
            # by_ref doesn't make sense, since this can't be assigned directly
            if by_ref:
                raise CompilerError(*expr.loc, '(ref x) cannot be used in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context, by_ref=True)
            return expr_dest, types.PointerTo(expr_type)
        elif isinstance(expr, expressions.Dereference):
            # by_ref makes sense in almost any context:
            #
            # (set (deref x) 5)
            #
            # This, for example, would modify the value at the pointer x
            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if not isinstance(expr_type, types.PointerTo):
                raise CompilerError(*expr.loc, '(deref x) requires x to be a non-function pointer')

            if by_ref:
                return expr_dest, self._resolve_if_type_name(expr_type.type)
            else:
                type_size = self._type_size(expr_type.type)
                type_align = self._type_alignment(expr_type.type)
                dest_offset = temp_context.add_temp(type_size, type_align)

                self._write_instr('    lw $t0, {}($fp)', expr_dest)
                self._memcpy_opt('t1', expr_type.type,
                    't0', 0, 
                    'fp', dest_offset)

                return dest_offset, self._resolve_if_type_name(expr_type.type)
        elif isinstance(expr, expressions.PointerToInt):
            # by_ref doesn't make sense - an integer can't be assigned to
            if by_ref:
                raise CompilerError(*expr.loc, '(ptr-to-int x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if not isinstance(expr_type, types.PointerTo):
                raise CompilerError(*expr.loc, '(ptr-to-int x) requires x to be a non-function pointer')

            return expr_dest, types.Integer
        elif isinstance(expr, expressions.IntToPointer):
            # by_ref doesn't make sense - can't assign to the result of an expression
            if by_ref:
                raise CompilerError(*expr.loc, '(int-to-ptr x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if expr_type is not types.Integer:
                raise CompilerError(*expr.loc, '(int-to-ptr x t) requires x to be an integer')

            ret_type = self._resolve_if_type_name(expr.type)
            if not isinstance(ret_type, types.PointerTo):
                raise CompilerError(*expr.loc, '(int-to-ptr x t) requires t to be pointer type')

            return expr_dest, ret_type
        elif isinstance(expr, expressions.IntToByte):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                raise CompilerError(*expr.loc, '(int-to-byte x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)
            
            if expr_type is not types.Integer:
                raise CompilerError(*expr.loc, '(int-to-byte x) requires x to be an integer')

            byte_size = self._type_size(types.Byte)
            byte_align = self._type_alignment(types.Byte)
            dest_offset = temp_context.add_temp(byte_size, byte_align)

            self._write_instr('    lw $t0, {}($fp)', expr_dest)
            self._write_instr('    sll $t0, $t0, 24')
            self._write_instr('    sra $t0, $t0, 24')
            self._write_instr('    sb $t0, {}($fp)', dest_offset)

            return dest_offset, types.Byte
        elif isinstance(expr, expressions.ByteToInt):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                raise CompilerError(*expr.loc, '(byte-to-int x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)
            
            if expr_type is not types.Byte:
                raise CompilerError(*expr.loc, '(byte-to-int x) requires x to be an byte')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            self._write_instr('    lb $t0, {}($fp)', expr_dest)
            self._write_instr('    sw $t0, {}($fp)', dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Cast):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                raise CompilerError(*expr.loc, '(cast t x) cannot be used in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if not isinstance(expr_type, types.PointerTo):
                raise CompilerError(*expr.loc, '(cast t x) requires x to be a pointer type')

            ret_type = self._resolve_if_type_name(expr.type)
            if not isinstance(ret_type, types.PointerTo):
                raise CompilerError(*expr.loc, '(cast t x) requires t to be pointer type')

            return expr_dest, ret_type
        elif isinstance(expr, expressions.Array):
            # by_ref works, since arrays can be assigned to
            array_dest, array_type = self._compile_expression(expr.array, temp_context)

            if not isinstance(array_type, types.PointerTo):
                raise CompilerError(*expr.loc, '(array x i) requires x to be a pointer type')

            index_dest, index_type = self._compile_expression(expr.index, temp_context)
    
            if index_type is not types.Integer: 
                raise CompilerError(*expr.loc, '(array x i) requires i to be an integer')

            # We have to account for the fact that the memory used by an element
            # also includes the alignment padding added
            element_type = self._resolve_if_type_name(array_type.type)
            element_align = self._type_alignment(element_type)
            raw_element_size = self._type_size(element_type)
            element_size = types.align_address(raw_element_size, element_align)

            if by_ref:
                int_size = self._type_size(types.Integer)
                int_align = self._type_alignment(types.Integer)
                dest_offset = temp_context.add_temp(int_size, int_align)
            else:
                dest_offset = temp_context.add_temp(raw_element_size, element_align)

            # This is an address computation for getting the precise offset to
            # the element in question
            #
            #  base := load(array_dest)
            #  index := load(index_dest)
            #  byte_offset := index_dest * element_size
            #  result := base + byte_offset
            self._write_instr('    lw $t0, {}($fp)', array_dest)
            self._write_instr('    lw $t1, {}($fp)', index_dest)
            self._write_instr('    li $t2, {}', element_size)
            self._write_instr('    mult $t1, $t2')
            self._write_instr('    mflo $t1')
            self._write_instr('    add $t0, $t0, $t1')

            if by_ref:
                self._write_instr('    sw $t0, {}($fp)', dest_offset)
                return dest_offset, element_type
            else:
                self._memcpy_opt('t1', element_type,
                    't0', 0,
                    'fp', dest_offset)

                return dest_offset, element_type
        elif isinstance(expr, expressions.Field):
            # by_ref makes sense for fields, since the address of the final
            # field can be assigned to

            # Note that intermediate fields can always be loaded by reference,
            # since structs always have memory locations
            struct_dest, struct_type = (
                self._compile_expression(expr.struct, temp_context, by_ref=True))

            if not isinstance(struct_type, types.Struct):
                raise CompilerError(*expr.loc, '(field s f...) requires that s be a structure')

            self._write_instr('    lw $t0, {}($fp)', struct_dest)
            total_offset = 0

            for field_name in expr.fields:
                try:
                    total_offset += self._field_offset(struct_type, field_name)
                except KeyError:
                    raise CompilerError(*expr.loc, 
                        "No field '{}' in structure {}", field_name, struct_type)

                struct_type = self._resolve_if_type_name(struct_type.fields[field_name])

            if by_ref:
                ref_type_size = self._type_size(types.Integer)
                ref_type_align = self._type_alignment(types.Integer)
                dest_offset = temp_context.add_temp(ref_type_size, ref_type_align)

                self._write_instr('    add $t0, $t0, {}', total_offset)
                self._write_instr('    sw $t0, {}($fp)', dest_offset)
                return dest_offset, struct_type
            else:
                last_field_size = self._type_size(struct_type)
                last_field_align = self._type_alignment(struct_type)
                dest_offset = temp_context.add_temp(last_field_size, last_field_align)

                self._memcpy_opt('t1', struct_type,
                    't0', total_offset,
                    'fp', dest_offset)

                return dest_offset, struct_type
        elif isinstance(expr, expressions.Arithmetic):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use arithmetic result in a ref context')

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            rhs_dest, rhs_type = self._compile_expression(expr.rhs, temp_context)
            
            if lhs_type is not types.Integer:
                raise CompilerError(*expr.loc, 
                    'Arithmetic expression requires integer on LHS')

            if rhs_type is not types.Integer:
                raise CompilerError(*expr.loc, 
                    'Arithmetic expression requires integer on RHS')
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            self._write_instr('    lw $t0, {}($fp)', lhs_dest)
            self._write_instr('    lw $t1, {}($fp)', rhs_dest)

            if expr.kind == expressions.ARITH_PLUS:
                self._write_instr('    add $t0, $t0, $t1')
            elif expr.kind == expressions.ARITH_MINUS:
                self._write_instr('    sub $t0, $t0, $t1')
            elif expr.kind == expressions.ARITH_TIMES:
                self._write_instr('    mult $t0, $t1')
                self._write_instr('    mflo $t0')
            elif expr.kind == expressions.ARITH_DIVIDE:
                self._write_instr('    div $t0, $t1')
                self._write_instr('    mflo $t0')
            elif expr.kind == expressions.ARITH_MOD:
                self._write_instr('    div $t0, $t1')
                self._write_instr('    mfhi $t0')

            self._write_instr('    sw $t0, {}($fp)', dest_offset)
            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Compare):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use & result in a ref context')

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            rhs_dest, rhs_type = self._compile_expression(expr.rhs, temp_context)
            
            if not isinstance(lhs_type, (types.IntegerType, types.PointerTo)):
                raise CompilerError(*expr.loc, 
                    'Comparison expression requires integer or pointer on LHS')

            if not isinstance(rhs_type, (types.IntegerType, types.PointerTo)):
                raise CompilerError(*expr.loc, 
                    'Comparison expression requires integer or pointer on RHS')
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            self._write_instr('    lw $t0, {}($fp)', lhs_dest)
            self._write_instr('    lw $t1, {}($fp)', rhs_dest)

            if expr.kind == expressions.CMP_LESS:
                self._write_instr('    slt $t0, $t0, $t1')
            elif expr.kind == expressions.CMP_GREATER:
                self._write_instr('    slt $t0, $t1, $t0')
            elif expr.kind == expressions.CMP_LESSEQ:
                self._write_instr('    sle $t0, $t0, $t1')
            elif expr.kind == expressions.CMP_GREATEQ:
                self._write_instr('    sle $t0, $t1, $t0')
            elif expr.kind == expressions.CMP_EQ:
                self._write_instr('    seq $t0, $t0, $t1')
            elif expr.kind == expressions.CMP_NOTEQ:
                self._write_instr('    sne $t0, $t0, $t1')

            self._write_instr('    sw $t0, {}($fp)', dest_offset)
            return dest_offset, types.Integer
        elif isinstance(expr, (expressions.BitAnd, expressions.BitOr, expressions.BitXor,
                                expressions.BitShiftLeft, expressions.BitShiftRight)):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use bitwise result in a ref context')

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            rhs_dest, rhs_type = self._compile_expression(expr.rhs, temp_context)
            
            if lhs_type is not types.Integer:
                raise CompilerError(*expr.loc, 
                    'Bitwise expression requires integer on LHS')

            if rhs_type is not types.Integer:
                raise CompilerError(*expr.loc, 
                    'Bitwise expression requires integer on RHS')
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            self._write_instr('    lw $t0, {}($fp)', lhs_dest)
            self._write_instr('    lw $t1, {}($fp)', rhs_dest)

            if isinstance(expr, expressions.BitAnd):
                self._write_instr('    and $t0, $t0, $t1')
            elif isinstance(expr, expressions.BitOr):
                self._write_instr('    or $t0, $t0, $t1')
            elif isinstance(expr, expressions.BitXor):
                self._write_instr('    xor $t0, $t0, $t1')
            elif isinstance(expr, expressions.BitShiftLeft):
                self._write_instr('    sllv $t0, $t0, $t1')
            elif isinstance(expr, expressions.BitShiftRight):
                if expr.sign_extend:
                    self._write_instr('    srav $t0, $t0, $t1')
                else:
                    self._write_instr('    srlv $t0, $t0, $t1')

            self._write_instr('    sw $t0, {}($fp)', dest_offset)
            return dest_offset, types.Integer
        elif isinstance(expr, expressions.BitNot):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use bitwise result in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if expr_type is not types.Integer:
                raise CompilerError(*expr.loc,
                    'Bitwise expression requires integer')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            self._write_instr('    lw $t0, {}($fp)', expr_dest)
            self._write_instr('    nor $t0, $t0, $0')
            self._write_instr('    sw $t0, {}($fp)', dest_offset)
            return dest_offset, types.Integer

        elif isinstance(expr, expressions.And):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use logical result in a ref context')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            # The code that we're going for here is the equivalent of the following:
            #
            # Given X := A && B
            #
            # a_result := A
            # X := 0
            # if (a_result)
            #     b_result := B
            #     X := b_result
            end_label = next(LABEL_MAKER)

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            
            if lhs_type is not types.Integer:
                raise CompilerError(*expr.loc, 
                    'Logical expression requires integer on LHS')

            self._write_instr('    lw $t0, {}($fp)', lhs_dest)
            self._write_instr('    beq $t0, $0, {}', end_label)

            # The reason for the sub-context here is that there is an extra stack
            # allocation in the case where the expression doesn't short circuit,
            # and we don't want the stack to become inconsistent across the branch
            sub_context = temp_context.get_temp_context()
            with sub_context:
                rhs_dest, rhs_type = self._compile_expression(expr.rhs, sub_context)

                if rhs_type is not types.Integer:
                    raise CompilerError(*expr.loc, 
                        'Logical expression requires integer on RHS')

                self._write_instr('    lw $t0, {}($fp)', rhs_dest)

            self._write_instr('{}:', end_label)
            self._write_instr('    sw $t0, {}($fp)', dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Or):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use logical result in a ref context')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            # The code that we're going for here is the equivalent of the following:
            #
            # Given X := A || B
            #
            # a_result := A
            # X := a_result
            # if (!a_result)
            #     b_result := B
            #     X := b_result
            end_label = next(LABEL_MAKER)

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            
            if lhs_type is not types.Integer:
                raise CompilerError(*expr.loc, 
                    'Logical expression requires integer on LHS')

            self._write_instr('    lw $t0, {}($fp)', lhs_dest)
            self._write_instr('    bne $t0, $0, {}', end_label)

            sub_context = temp_context.get_temp_context()
            with sub_context:
                rhs_dest, rhs_type = self._compile_expression(expr.rhs, sub_context)

                if rhs_type is not types.Integer:
                    raise CompilerError(*expr.loc, 
                        'Logical expression requires integer on RHS')

                self._write_instr('    lw $t0, {}($fp)', rhs_dest)

            self._write_instr('{}:', end_label)
            self._write_instr('    sw $t0, {}($fp)', dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Not):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use logical result in a ref context')

            expr_dest, expr_type = (
                self._compile_expression(expr.expr, temp_context))

            if expr_type is not types.Integer:
                raise CompilerError(*expr.loc, 'Logical expression requires integer')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            self._write_instr('    lw $t0, {}($fp)', expr_dest)
            self._write_instr('    seq $t0, $t0, $0')
            self._write_instr('    sw $t0, {}($fp)', dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.SizeOf):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use size-of result in a ref context')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            type_obj = self._resolve_if_type_name(expr.type)
            self._write_instr('    li $t0, {}', self._type_size(type_obj))
            self._write_instr('    sw $t0, {}($fp)', dest_offset)
            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Call):
            if by_ref:
                raise CompilerError(*expr.loc, 'Cannot use function result in a ref context')

            func_dest, func_type = self._compile_expression(expr.func, temp_context)

            if not isinstance(func_type, types.FunctionPointer):
                raise CompilerError(*expr.loc, 'Calls must be either to functions or function pointers')

            if len(expr.params) != len(func_type.params):
                raise CompilerError(*expr.loc, '{} expected {} params, got {}', 
                        func_type, len(func_type.params), len(expr.params))

            rev_param_dests = []
            rev_param_types = []

            func_params = (self._resolve_if_type_name(param_type) for param_type in func_type.params)
            for param, param_expected_type in reversed(list(zip(expr.params, func_params))):
                param_dest, param_real_type = self._compile_expression(param, temp_context)

                if param_real_type != param_expected_type:
                    raise CompilerError(*expr.loc, '{} expected in call, got {}', 
                        param_expected_type, param_real_type)

                rev_param_dests.append(param_dest)
                rev_param_types.append(param_real_type)

            # The reason for going through the parameter list again, is to
            # ensure that they make it to the end of the stack 
            # (successive param_dest values may not be successive, which we
            # need them to be in order for the called function to know where
            # they are)
            for param_dest, param_type in zip(rev_param_dests, rev_param_types):
                type_size = self._type_size(param_type)
                type_align = self._type_alignment(param_type)
                copy_dest = temp_context.add_temp(type_size, type_align)

                self._memcpy_opt('t1', param_type,
                    'fp', param_dest,
                    'fp', copy_dest)

            self._write_instr('    lw $t0, {}($fp)', func_dest)
            self._write_instr('    jalr $t0')

            return_type = self._resolve_if_type_name(func_type.return_type)
            return_type_size = self._type_size(return_type)
            return_type_alignment = self._type_alignment(return_type)

            return_dest = temp_context.add_temp(return_type_size, return_type_alignment)
            
            # Since structure return types are not allowed, the most we'll be
            # copying is a full word
            instr = {
                1: 'sb',
                2: 'sh',
                4: 'sw',
            }[return_type_size]
            self._write_instr('    {} $v0, {}($fp)', instr, return_dest)

            return return_dest, return_type

    def handle_set(self, assignable, expression):
        """
        Handles an assignment from an expression to an assignable target.
        """
        self._write_comment('==== Assign ====')
        self._write_comment('  From: {}', expression)
        self._write_comment('  To: {}', assignable)

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            assign_dest, assign_type = (
                self._compile_expression(assignable, temp_context, by_ref=True))

            value_dest, value_type = (
                self._compile_expression(expression, temp_context))

            if value_type != assign_type:
                raise CompilerError(self.line, self.col,
                    'Cannot assign {} to {}', value_type, assign_type)

            # We have to do a dereference here, since the effect of loading
            # by_ref is that we get an address rather than a value
            self._write_instr('    lw $t0, {}($fp)', assign_dest)

            assign_size = self._type_size(assign_type)
            self._memcpy_opt('t1', assign_type,
                'fp', value_dest,
                't0', 0)

    def handle_if(self, cond):
        """
        Handles the start of an if statement.
        """
        self._write_comment('====== If ======')
        self._write_comment('  Condition: {}', cond)

        if_context = IfLabels(next(LABEL_MAKER), next(LABEL_MAKER))
        self.if_labels.append(if_context)

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            cond_dest, cond_type = (
                self._compile_expression(cond, temp_context))

            if cond_type is not types.Integer:
                raise CompilerError(*cond.loc, 'Conditional must be an integer')

            self._write_instr('    lw $t0, {}($fp)', cond_dest)

        # The position outside the context is deliberate - we have to avoid
        # any situations where the code doesn't execute the stack adjustment
        # code. In this case, indenting this write call will leave the stack
        # deeper than it should be, if the branch is taken
        self._write_instr('    beq $t0, $0, {}', if_context.else_body)

    def handle_else(self):
        """
        Handles the end of the 'then' part of an if statement
        """
        self._write_comment('====== Else ======')

        if_context = self.if_labels[-1]
        self._write_instr('    j {}', if_context.end)
        self._write_instr('{}:', if_context.else_body)

    def handle_if_end(self):
        """
        Handles the end of an if block
        """
        self._write_comment('====== End If ======')

        if_context = self.if_labels[-1]
        self._write_instr('{}:', if_context.end)
        self.if_labels.pop()

    def handle_while(self, cond):
        """
        Handles the start of a while loop.
        """
        self._write_comment('====== While ======')
        self._write_comment('  Condition: {}', cond)

        while_context = WhileLabels(next(LABEL_MAKER), next(LABEL_MAKER))
        self.while_labels.append(while_context)

        self._write_instr('{}:', while_context.cond)

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            cond_dest, cond_type = (
                self._compile_expression(cond, temp_context))

            if cond_type is not types.Integer:
                raise CompilerError(*cond.loc, 'Conditional must be an integer')

            self._write_instr('    lw $t0, {}($fp)', cond_dest)

        # The position outside the context is deliberate - we have to avoid
        # any situations where the code doesn't execute the stack adjustment
        # code. In this case, indenting this write call will leave the stack
        # deeper than it should be, if the branch is taken
        self._write_instr('    beq $t0, $0, {}', while_context.exit)

    def handle_while_end(self):
        """
        Handles the end of a while loop.
        """
        self._write_comment('====== While End ======')

        while_context = self.while_labels[-1]
        self._write_instr('    j {}', while_context.cond)
        self._write_instr('{}:', while_context.exit)

    def handle_break(self):
        """
        Handles breaking out of a while loop.
        """
        self._write_comment('==== Break ====')
        try:
            while_context = self.while_labels[-1]
        except IndexError:
            raise CompilerError(self.line, self.col, 'Cannot have break outside of while loop')

        self._write_instr('    j {}', while_context.exit)

    def handle_continue(self):
        """
        Handles continuing at the top of a while loop.
        """
        self._write_comment('==== Continue  ====')
        try:
            while_context = self.while_labels[-1]
        except IndexError:
            raise CompilerError(self.line, self.col, 'Cannot have break outside of while loop')

        self._write_instr('    j {}', while_context.cond)

    def handle_return(self, expr):
        """
        Handles returning values from functions.
        """
        self._write_comment('==== Return ====')
        self._write_comment('  Return: {}', expr)

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            ret_dest, ret_type = (
                self._compile_expression(expr, temp_context))

            if ret_type != self.func_ret_type:
                raise CompilerError(*expr.loc, 'Returned expression must be the same as the function return type')

            # Since we're barred from returning structures, the most we'll
            # have to deal with is words
            ret_type_size = self._type_size(ret_type)

            instr = {
                1: 'lb',
                2: 'lh',
                4: 'lw',
            }[ret_type_size]
            self._write_instr('    {} $v0, {}($fp)', instr, ret_dest)

        self._write_instr('    j {}', self.func_exit_label)

    def handle_raw_expression(self, expr):
        """
        Handles an expression that isn't enclosed in a statement.
        """
        self._write_comment('==== Raw Expression ====')
        self._write_comment('  Expression: {}', expr)

        self.update_position(*expr.loc)

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            self._compile_expression(expr, temp_context)

def get_backend(output, is_library):
    """
    Returns the backend represented by this module.
    """
    return MarsBackend(output, is_library)
