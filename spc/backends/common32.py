"""
A common backend for 32-bit processors.
"""
import logging

from ..backend import BaseBackend
from ..backend_utils import (
    comment_after, ContextMixin, IfLabels, ThirtyTwoMixin, WhileLabels,
)
from .. import expressions
from ..require_processor import RequireProcessor
from ..symbols import SymbolTable
from .. import types
from ..util import (
    make_label_maker, mangle_label, unescape_bytes
)

LOGGER = logging.getLogger('spc.common32')

BUILTIN_TYPES = SymbolTable(is_builtin=True)
BUILTIN_TYPES['string'] = types.PointerTo(types.Byte)

BUILTIN_FUNCTIONS = SymbolTable(is_builtin=True)


class Common32Backend(ContextMixin, ThirtyTwoMixin, BaseBackend):
    """
    Emits MIPS assembly code compatible with the MARS simulator.
    """
    def __init__(self, templates, output, filename, is_library):
        BaseBackend.__init__(self, output, filename, is_library,
                BUILTIN_FUNCTIONS, BUILTIN_TYPES)

        ContextMixin.__init__(self)

        self.templates = templates
        self.exported = set()
        self.undefined_funcs = set()
        self.comment_fmt = templates.comment_fmt
        self.label_maker = make_label_maker()


        # It's important for the templates to get to the backend, so they can
        # actually spit out code
        self.templates.backend = self

    def _make_func_stack(self):
        return self.templates.make_func_stack()

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
            self.error(0, 0,
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
                self.error(self.line, self.col, 'Cannot have >1 top-level declaration blocks')

            self.read_top_decls = True
            self.templates.emit_data_segment()
        else:
            if self.read_func_decls:
                self.error(self.line, self.col, 'Cannot have >1 function declaration blocks')

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

                self.templates.emit_stack_string(base_addr, escaped)
            else:
                self.templates.emit_label(mangle_label(name))
                self.templates.emit_static_string(decl_type.bytes.decode('ascii'))
        elif was_type_name or isinstance(decl_type, types.RAW_TYPES):
            self._write_comment('  Declaring variable {} :: {}', name, decl_type)

            was_array = isinstance(decl_type, types.ArrayOf)
            self.current_context.value_defns[name] = types.decay_if_array(decl_type)

            if self.in_function:
                # Raw types have to be allocated stack space
                size = self._type_size(decl_type)
                alignment = self._type_alignment(decl_type)
                self.current_context.func_stack.add_local(name, size, alignment)
            else:
                # Raw types have to be allocated space inside of .data
                # and given a label
                self._write_comment('  As a global')

                size = self._type_size(decl_type)
                self.templates.emit_label(mangle_label(name))
                self.templates.emit_static_space(size)
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
                self.error(self.line, self.col, 'Cannot declare nested functions')

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
            self.current_context.func_stack.expand_locals()
        else:
            self.templates.emit_text_segment()
            if not self.library:
                self.templates.emit_prog_header()

    def handle_require(self, filename):
        """
        Handles a require which loads the given filename.
        """
        if self.in_function:
            self.error(self.line, self.col,
                "Cannot load another file inside of a function")

        try:
            processor = RequireProcessor.require(filename)
            if processor is None:
                return

            for type_name, type_obj in processor.exported_types:
                self._write_comment('Importing definition: {} of type {}', type_name, type_obj)
                self.current_context.type_defns[type_name] = type_obj

            for val_name, val_obj in processor.exported_values:
                self._write_comment('Importing value: {} of type {}', val_name, val_obj)
                self.current_context.value_defns[val_name] = val_obj
                self.exported.add(val_name)

            for arr_name, arr_obj in processor.exported_arrays:
                self.current_context.array_bound[arr_name] = True
        except OSError:
            self.error(self.line, self.col,
                "Could not open file '{}' for reading", filename)

    def handle_exports(self, names):
        """
        Handles an export block.
        
        This will mark a value as either .extern (if it is a value) or as 
        .globl (if it is a function).

        This is invalid at the function level, because values inner to 
        functions aren't available outside that function.
        """
        if self.in_function:
            self.error(self.line, self.col,
                'Cannot import values inside of function')

        for name in names:
            if name in self.exported:
                self.error(self.line, self.col,
                    'Cannot re-export foreign value "{}"', name)

            try:
                type_of = self.current_context.value_defns[name]
            except KeyError:
                self.error(self.line, self.col,
                    'Undefined export "{}"', name)

            if not types.can_be_global(type_of):
                self.error(self.line, self.col,
                    'Cannot export value of type "{}"', type_of)

            self.templates.emit_export(name, type_of)

    def handle_func_def_start(self, name, params):
        """
        Handles the beginning of a function.
        """
        self.in_function = True
        self.func_exit_label = next(self.label_maker)

        try:
            self.undefined_funcs.remove(name)
            func_defn = self.current_context.value_defns[name]
        except KeyError as exn:
            self.error(self.line, self.col, 'Undefined function "{}"', name)

        if not isinstance(func_defn, types.FunctionDecl):
            self.error(self.line, self.col, 'Value {} is not a function', name)

        self._push_context()

        if len(params) != len(func_defn.params):
            self.error(self.line, self.col, 'Type has {} params, definition has {}',
                len(func_defn.params), len(params))

        self._write_comment('== Binding parameters ==')

        # The reason for padding both above and below the params is explained
        # in the code which handles the Call expression 

        first_arg = True
        last_alignment = None
        for param, param_type in zip(params, func_defn.params):
            param_type = self._resolve_if_type_name(param_type)

            type_size = self._type_size(param_type)
            alignment = self._type_alignment(param_type)
            last_alignment = alignment
            
            if first_arg and alignment % 4 != 0:
                self._write_comment('Head Padding {} bytes', 4 - (alignment % 4))
                self.current_context.func_stack.pad_param(4 - (alignment % 4))
                first_arg = False

            self.current_context.func_stack.add_param(param, type_size, alignment)
            self.current_context.value_defns[param] = param_type

        if last_alignment is not None and last_alignment % 4 != 0:
            self._write_comment('Tail Padding {} bytes', 4 - (last_alignment % 4))
            self.current_context.pad_param(4 - (last_alignment % 4))

        self.func_ret_type = self._resolve_if_type_name(func_defn.return_type)
        self.templates.emit_label(mangle_label(name))
        self.templates.emit_func_header(name)

    def handle_func_def_end(self):
        """
        Handles the end of the function definition.

        Writes out the end label and the code for doing a function return.
        """
        self.templates.emit_label(self.func_exit_label)
        self.current_context.func_stack.cleanup_locals()
        self.templates.emit_func_footer()
        self._pop_context()

        self.in_function = False
        self.read_func_decls = False
        self.func_exit_label = None
        self.func_ret_type = None

    def handle_assembly(self, name, code):
        """
        Handles the definition of a function with inline assembly.
        """
        self.undefined_funcs.remove(name)

        try:
            func_defn = self.current_context.value_defns[name]
        except KeyError as exn:
            self.error(self.line, self.col, 'Undefined function "{}"', name)

        if not isinstance(func_defn, types.FunctionDecl):
            self.error(self.line, self.col, 'Value {} is not a function', name)

        self.templates.emit_label(mangle_label(name))
        self.templates.emit_asm(name, code)

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
            self.templates.emit_move_word(
                    tmp_reg, 
                    src_start_reg, 
                    src_start_offset, 
                    dest_start_reg, 
                    dest_start_offset)
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
            self.templates.emit_move_byte(
                    tmp_reg,
                    src_start_reg,
                    src_start_offset + byte,
                    dest_start_reg,
                    dest_start_offset + byte)

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
                self.error(*expr.loc, 'No variable "{}" in scope', expr.name)

            type_of = owning_scope[expr.name]
            if isinstance(type_of, types.TypeName):
                type_of = self._resolve_if_type_name(type_of)

            if isinstance(type_of, types.FunctionDecl):
                if by_ref:
                    # You can't logically assign to the address you get back, so
                    # it doesn't make sense to even try
                    self.error(*expr.loc, 'Cannot use function in a ref context')

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
                    self.error(*expr.loc, 'Cannot use array in a ref context')

                self._write_comment('  Variable load: array {}', expr.name)
                by_ref = True

            if by_ref:
                type_size = self._type_size(types.Integer)
                type_align = self._type_alignment(types.Integer)
            else:
                type_size = self._type_size(type_of)
                type_align = self._type_alignment(type_of)

            dest_offset = temp_context.add_temp(type_size, type_align)
            tmp_reg = self.templates.tmp_regs[0]

            if owning_scope.is_global:
                # Global variables are somewhere in .data land, labeled by their name
                self.templates.emit_load_static_addr(tmp_reg, expr.name)
            elif owning_scope.is_builtin:
                self.error(*expr.loc, 'Builtin values cannot be used, except to be called')
            else:
                # Local variables are on the stack
                stack_offset = self.current_context.func_stack[expr.name]
                self.templates.emit_load_stack_addr(tmp_reg, stack_offset)

            if by_ref:
                self.templates.emit_save_stack_word(tmp_reg, dest_offset)
            else:
                copy_reg = self.templates.tmp_regs[1]
                self._memcpy_opt(copy_reg, type_of,
                    tmp_reg, 0, 
                    self.templates.frame_reg, dest_offset)

            return dest_offset, type_of
        elif isinstance(expr, expressions.Integer):
            if by_ref:
                # by_ref is invalid, clearly, since you can't assign to an integer
                self.error(*expr.loc, 'Cannot use an integer literal in a ref context')

            dest_offset = temp_context.add_temp(self._type_size(types.Integer),
                                                self._type_alignment(types.Integer))


            tmp_reg = self.templates.tmp_regs[0]
            self.templates.emit_load_int(tmp_reg, expr.integer)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Reference):
            # by_ref doesn't make sense, since this can't be assigned directly
            if by_ref:
                self.error(*expr.loc, '(ref x) cannot be used in a ref context')

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
                self.error(*expr.loc, '(deref x) requires x to be a non-function pointer')

            if by_ref:
                return expr_dest, self._resolve_if_type_name(expr_type.type)
            else:
                type_size = self._type_size(expr_type.type)
                type_align = self._type_alignment(expr_type.type)
                dest_offset = temp_context.add_temp(type_size, type_align)

                tmp_reg = self.templates.tmp_regs[0]
                copy_reg = self.templates.tmp_regs[1]

                self.templates.emit_load_stack_word(tmp_reg, expr_dest)
                self._memcpy_opt(copy_reg, expr_type.type,
                    tmp_reg, 0, 
                    self.templates.frame_reg, dest_offset)

                return dest_offset, self._resolve_if_type_name(expr_type.type)
        elif isinstance(expr, expressions.PointerToInt):
            # by_ref doesn't make sense - an integer can't be assigned to
            if by_ref:
                self.error(*expr.loc, '(ptr-to-int x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if not isinstance(expr_type, types.PointerTo):
                self.error(*expr.loc, '(ptr-to-int x) requires x to be a non-function pointer')

            return expr_dest, types.Integer
        elif isinstance(expr, expressions.IntToPointer):
            # by_ref doesn't make sense - can't assign to the result of an expression
            if by_ref:
                self.error(*expr.loc, '(int-to-ptr x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if expr_type is not types.Integer:
                self.error(*expr.loc, '(int-to-ptr x t) requires x to be an integer')

            ret_type = self._resolve_if_type_name(expr.type)
            if not isinstance(ret_type, types.PointerTo):
                self.error(*expr.loc, '(int-to-ptr x t) requires t to be pointer type')

            return expr_dest, ret_type
        elif isinstance(expr, expressions.IntToByte):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                self.error(*expr.loc, '(int-to-byte x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)
            
            if expr_type is not types.Integer:
                self.error(*expr.loc, '(int-to-byte x) requires x to be an integer')

            byte_size = self._type_size(types.Byte)
            byte_align = self._type_alignment(types.Byte)
            dest_offset = temp_context.add_temp(byte_size, byte_align)
            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, expr_dest)
            self.templates.emit_int_to_byte(tmp_reg)
            self.templates.emit_save_stack_byte(tmp_reg, dest_offset)

            return dest_offset, types.Byte
        elif isinstance(expr, expressions.ByteToInt):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                self.error(*expr.loc, '(byte-to-int x) is not valid in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)
            
            if expr_type is not types.Byte:
                self.error(*expr.loc, '(byte-to-int x) requires x to be an byte')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)
            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_byte(tmp_reg, expr_dest)
            self.templates.emit_byte_to_int(tmp_reg)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Cast):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                self.error(*expr.loc, '(cast t x) cannot be used in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if not isinstance(expr_type, types.PointerTo):
                self.error(*expr.loc, '(cast t x) requires x to be a pointer type')

            ret_type = self._resolve_if_type_name(expr.type)
            if not isinstance(ret_type, types.PointerTo):
                self.error(*expr.loc, '(cast t x) requires t to be pointer type')

            return expr_dest, ret_type
        elif isinstance(expr, expressions.Array):
            # by_ref works, since arrays can be assigned to
            array_dest, array_type = self._compile_expression(expr.array, temp_context)

            if not isinstance(array_type, types.PointerTo):
                self.error(*expr.loc, '(array x i) requires x to be a pointer type')

            index_dest, index_type = self._compile_expression(expr.index, temp_context)
    
            if index_type is not types.Integer: 
                self.error(*expr.loc, '(array x i) requires i to be an integer')

            element_type = self._resolve_if_type_name(array_type.type)
            element_align = self._type_alignment(element_type)
            raw_element_size, pad_element_size = types.elem_size_with_padding(self, array_type)
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
            #  byte_offset := index_dest * pad_element_size
            #  result := base + byte_offset
            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_array_offset(tmp_reg, array_dest, index_dest, pad_element_size)

            if by_ref:
                self.templates.emit_save_stack_word(tmp_reg, dest_offset)
            else:
                copy_reg = self.templates.tmp_regs[1]
                self._memcpy_opt(copy_reg, element_type,
                    tmp_reg, 0,
                    self.templates.frame_reg, dest_offset)

            return dest_offset, element_type
        elif isinstance(expr, expressions.Field):
            # Note that intermediate fields can always be loaded by reference,
            # since structs always have memory locations
            struct_dest, struct_type = (
                self._compile_expression(expr.struct, temp_context, by_ref=True))

            if not isinstance(struct_type, types.Struct):
                self.error(*expr.loc, '(field s f...) requires that s be a structure')

            tmp_reg = self.templates.tmp_regs[0]
            self.templates.emit_load_stack_word(tmp_reg, struct_dest)

            total_offset = 0
            for field_name in expr.fields:
                try:
                    total_offset += self._field_offset(struct_type, field_name)
                except KeyError:
                    self.error(*expr.loc, 
                        "No field '{}' in structure {}", field_name, struct_type)

                struct_type = self._resolve_if_type_name(struct_type.fields[field_name])

            if by_ref:
                ref_type_size = self._type_size(types.Integer)
                ref_type_align = self._type_alignment(types.Integer)
                dest_offset = temp_context.add_temp(ref_type_size, ref_type_align)

                self.templates.emit_struct_offset(tmp_reg, total_offset)
                self.templates.emit_save_stack_word(tmp_reg, dest_offset)

                return dest_offset, struct_type
            else:
                last_field_size = self._type_size(struct_type)
                last_field_align = self._type_alignment(struct_type)
                dest_offset = temp_context.add_temp(last_field_size, last_field_align)

                copy_reg = self.templates.tmp_regs[1]
                self._memcpy_opt(copy_reg, struct_type,
                    tmp_reg, total_offset,
                    self.templates.frame_reg, dest_offset)

                return dest_offset, struct_type
        elif isinstance(expr, expressions.Arithmetic):
            if by_ref:
                self.error(*expr.loc, 'Cannot use arithmetic result in a ref context')

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            rhs_dest, rhs_type = self._compile_expression(expr.rhs, temp_context)
            
            if lhs_type is not types.Integer:
                self.error(*expr.loc, 
                    'Arithmetic expression requires integer on LHS')

            if rhs_type is not types.Integer:
                self.error(*expr.loc, 
                    'Arithmetic expression requires integer on RHS')
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            lhs_reg = self.templates.tmp_regs[0]
            tmp_reg = self.templates.tmp_regs[1]

            self.templates.emit_load_stack_word(lhs_reg, lhs_dest)
            self.templates.emit_load_stack_word(tmp_reg, rhs_dest)

            if expr.kind == expressions.ARITH_PLUS:
                self.templates.emit_add(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_MINUS:
                self.templates.emit_sub(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_TIMES:
                self.templates.emit_mul(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_DIVIDE:
                self.templates.emit_div(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_MOD:
                self.templates.emit_mod(lhs_reg, tmp_reg)

            self.templates.emit_save_stack_word(lhs_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Compare):
            if by_ref:
                self.error(*expr.loc, 'Cannot use & result in a ref context')

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            rhs_dest, rhs_type = self._compile_expression(expr.rhs, temp_context)
            
            if not isinstance(lhs_type, (types.IntegerType, types.PointerTo)):
                self.error(*expr.loc, 
                    'Comparison expression requires integer or pointer on LHS')

            if not isinstance(rhs_type, (types.IntegerType, types.PointerTo)):
                self.error(*expr.loc, 
                    'Comparison expression requires integer or pointer on RHS')
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            lhs_reg = self.templates.tmp_regs[0]
            tmp_reg = self.templates.tmp_regs[1]

            self.templates.emit_load_stack_word(lhs_reg, lhs_dest)
            self.templates.emit_load_stack_word(tmp_reg, rhs_dest)

            if expr.kind == expressions.CMP_LESS:
                self.templates.emit_less(lhs_reg, tmp_reg)
            elif expr.kind == expressions.CMP_GREATER:
                self.templates.emit_greater(lhs_reg, tmp_reg)
            elif expr.kind == expressions.CMP_LESSEQ:
                self.templates.emit_lesseq(lhs_reg, tmp_reg)
            elif expr.kind == expressions.CMP_GREATEQ:
                self.templates.emit_greateq(lhs_reg, tmp_reg)
            elif expr.kind == expressions.CMP_EQ:
                self.templates.emit_eq(lhs_reg, tmp_reg)
            elif expr.kind == expressions.CMP_NOTEQ:
                self.templates.emit_noteq(lhs_reg, tmp_reg)

            self.templates.emit_save_stack_word(lhs_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, (expressions.BitAnd, expressions.BitOr, expressions.BitXor,
                                expressions.BitShiftLeft, expressions.BitShiftRight)):
            if by_ref:
                self.error(*expr.loc, 'Cannot use bitwise result in a ref context')

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            rhs_dest, rhs_type = self._compile_expression(expr.rhs, temp_context)

            self.templates.emit_load_stack_word(lhs_reg, lhs_dest)
            self.templates.emit_load_stack_word(tmp_reg, rhs_dest)
            
            if lhs_type is not types.Integer:
                self.error(*expr.loc, 
                    'Bitwise expression requires integer on LHS')

            if rhs_type is not types.Integer:
                self.error(*expr.loc, 
                    'Bitwise expression requires integer on RHS')
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            lhs_reg = self.templates.tmp_regs[0]
            tmp_reg = self.templates.tmp_regs[1]

            if isinstance(expr, expressions.BitAnd):
                self.templates.emit_bit_and(lhs_reg, tmp_reg)
            elif isinstance(expr, expressions.BitOr):
                self.templates.emit_bit_or(lhs_reg, tmp_reg)
            elif isinstance(expr, expressions.BitXor):
                self.templates.emit_xor(lhs_reg, tmp_reg)
            elif isinstance(expr, expressions.BitShiftLeft):
                self.templates.emit_shiftleft(lhs_reg, tmp_reg)
            elif isinstance(expr, expressions.BitShiftRight):
                if expr.sign_extend:
                    self.templates.emit_shiftright_arith(lhs_reg, tmp_reg)
                else:
                    self.templates.emit_shiftright_log(lhs_reg, tmp_reg)

            self.templates.emit_save_stack_word(lhs_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.BitNot):
            if by_ref:
                self.error(*expr.loc, 'Cannot use bitwise result in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            if expr_type is not types.Integer:
                self.error(*expr.loc,
                    'Bitwise expression requires integer')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, expr_dest)
            self.templates.emit_bit_not(tmp_reg)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer

        elif isinstance(expr, expressions.And):
            if by_ref:
                self.error(*expr.loc, 'Cannot use logical result in a ref context')

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
            end_label = next(self.label_maker)

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            
            if lhs_type is not types.Integer:
                self.error(*expr.loc, 
                    'Logical expression requires integer on LHS')

            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, lhs_dest)
            self.templates.emit_branch_if_zero(tmp_reg, end_label)

            # The reason for the sub-context here is that there is an extra stack
            # allocation in the case where the expression doesn't short circuit,
            # and we don't want the stack to become inconsistent across the branch
            sub_context = temp_context.get_temp_context()
            with sub_context:
                rhs_dest, rhs_type = self._compile_expression(expr.rhs, sub_context)

                if rhs_type is not types.Integer:
                    self.error(*expr.loc, 
                        'Logical expression requires integer on RHS')

                self.templates.emit_load_stack_word(tmp_reg, rhs_dest)

            self.templates.emit_label(end_label)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Or):
            if by_ref:
                self.error(*expr.loc, 'Cannot use logical result in a ref context')

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
            end_label = next(self.label_maker)

            lhs_dest, lhs_type = self._compile_expression(expr.lhs, temp_context)
            
            if lhs_type is not types.Integer:
                self.error(*expr.loc, 
                    'Logical expression requires integer on LHS')

            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, lhs_dest)
            self.templates.emit_branch_if_nonzero(tmp_reg, end_label)

            sub_context = temp_context.get_temp_context()
            with sub_context:
                rhs_dest, rhs_type = self._compile_expression(expr.rhs, sub_context)

                if rhs_type is not types.Integer:
                    self.error(*expr.loc, 
                        'Logical expression requires integer on RHS')

                self.templates.emit_load_stack_word(tmp_reg, rhs_dest)

            self.templates.emit_label(end_label)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Not):
            if by_ref:
                self.error(*expr.loc, 'Cannot use logical result in a ref context')

            expr_dest, expr_type = (
                self._compile_expression(expr.expr, temp_context))

            if expr_type is not types.Integer:
                self.error(*expr.loc, 'Logical expression requires integer')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)
            tmp_reg= self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, expr_dest)
            self.templates.emit_not(tmp_reg)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.SizeOf):
            if by_ref:
                self.error(*expr.loc, 'Cannot use size-of result in a ref context')

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)
            tmp_reg = self.templates.tmp_regs[0]
            type_obj = self._resolve_if_type_name(expr.type)

            self.templates.emit_load_int(tmp_reg, self._type_size(type_obj))
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Call):
            if by_ref:
                self.error(*expr.loc, 'Cannot use function result in a ref context')

            func_dest, func_type = self._compile_expression(expr.func, temp_context)

            if not isinstance(func_type, types.FunctionPointer):
                self.error(*expr.loc, 'Calls must be either to functions or function pointers')

            if len(expr.params) != len(func_type.params):
                self.error(*expr.loc, '{} expected {} params, got {}', 
                        func_type, len(func_type.params), len(expr.params))

            rev_param_dests = []
            rev_param_types = []

            func_params = (self._resolve_if_type_name(param_type) for param_type in func_type.params)
            for param, param_expected_type in reversed(list(zip(expr.params, func_params))):
                param_dest, param_real_type = self._compile_expression(param, temp_context)

                if param_real_type != param_expected_type:
                    self.error(*expr.loc, '{} expected in call, got {}', 
                        param_expected_type, param_real_type)

                rev_param_dests.append(param_dest)
                rev_param_types.append(param_real_type)

            # The reason for going through the parameter list again, is to
            # ensure that they make it to the end of the stack 
            # (the loop above doesn't necessarily put them in sequential order,
            # and we need them to be)

            # There are two subtle bits here that have to do with alignment:
            # 
            # 1. The last argument is always padded with enough space to word
            #    align where the function's saved $fp will go
            # 2. The first argument is always word aligned. This helps avoid
            #    issues like the following:
            #
            #
            # Caller locals: *, Args: A B, Padding: X, Callee $fp: $
            #
            #   |-|-|-|-|  |-|-|-|-|
            #   |*|B|B|B|  |*|*|*|B|
            #   |-|-|-|-|  |-|-|-|-|
            #   |A|A|A|X|  |B|B|A|A|
            #   |-|-|-|-|  |-|-|-|-|
            #   |$|$|$|$|  |A|X|X|X|
            #   |-|-|-|-|  |-|-|-|-|
            #   | | | | |  |$|$|$|$|
            #
            # Note the different amounts of padding - 1 byte vs 3 bytes. This
            # moves the offsets for the arguments around, which we don't want.
            # However, if we pad after the last arg *and* before the first:
            #
            #   |-|-|-|-|  |-|-|-|-|
            #   |*|X|X|X|  |*|*|*|X|
            #   |-|-|-|-|  |-|-|-|-|
            #   |X|B|B|B|  |X|B|B|B|
            #   |-|-|-|-|  |-|-|-|-|
            #   |A|A|A|X|  |A|A|A|X|
            #   |-|-|-|-|  |-|-|-|-|
            #   |$|$|$|$|  |$|$|$|$|
            #
            # Now, the argument offsets are constant, since they both use the
            # same amount of padding between the first arg and the $fp word.

            # This has the effect of padding the parameter storage location,
            # but only in the case that its current address isn't 4-byte aligned
            self._write_comment('-- Head Padding --')
            temp_context.add_temp(0, 4)

            tmp_reg = self.templates.tmp_regs[0]
            copy_reg = self.templates.tmp_regs[1]

            for param_dest, param_type in zip(rev_param_dests, rev_param_types):
                type_size = self._type_size(param_type)
                type_align = self._type_alignment(param_type)

                copy_dest = temp_context.add_temp(type_size, type_align)

                self._memcpy_opt(copy_reg, param_type,
                    self.templates.frame_reg, param_dest,
                    self.templates.frame_reg, copy_dest)

            self._write_comment('-- Tail Padding --')
            temp_context.add_temp(0, 4)

            self.templates.emit_load_stack_word(tmp_reg, func_dest)
            self.templates.emit_indirect_call(tmp_reg)

            return_type = self._resolve_if_type_name(func_type.return_type)
            return_type_size = self._type_size(return_type)
            return_type_alignment = self._type_alignment(return_type)

            return_dest = temp_context.add_temp(return_type_size, return_type_alignment)
            
            # Since structure return types are not allowed, the most we'll be
            # copying is a full word
            if return_type_size == 1:
                self.templates.emit_save_stack_byte(self.templates.return_reg, return_dest)
            elif return_type_size == 4:
                self.templates.emit_save_stack_word(self.templates.return_reg, return_dest)

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
                self.error(self.line, self.col,
                    'Cannot assign {} to {}', value_type, assign_type)

            # We have to do a dereference here, since the effect of loading
            # by_ref is that we get an address rather than a value
            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, assign_dest)

            assign_size = self._type_size(assign_type)
            copy_reg = self.templates.tmp_regs[1]

            self._memcpy_opt(copy_reg, assign_type,
                 self.templates.frame_reg, value_dest,
                 tmp_reg, 0)

    def handle_if(self, cond):
        """
        Handles the start of an if statement.
        """
        self._write_comment('====== If ======')
        self._write_comment('  Condition: {}', cond)

        if_context = IfLabels(
                next(self.label_maker), 
                next(self.label_maker))

        self.if_labels.append(if_context)

        tmp_reg = self.templates.tmp_regs[0]

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            cond_dest, cond_type = (
                self._compile_expression(cond, temp_context))

            if cond_type is not types.Integer:
                self.error(*cond.loc, 'Conditional must be an integer')

            self.templates.emit_load_stack_word(tmp_reg, cond_dest)

        # The position outside the context is deliberate - we have to avoid
        # any situations where the code doesn't execute the stack adjustment
        # code. In this case, indenting this write call will leave the stack
        # deeper than it should be, if the branch is taken
        self.templates.emit_branch_if_zero(tmp_reg, if_context.else_body)

    def handle_else(self):
        """
        Handles the end of the 'then' part of an if statement
        """
        self._write_comment('====== Else ======')

        if_context = self.if_labels[-1]
        self.templates.emit_jump(if_context.end)
        self.templates.emit_label(if_context.else_body)

    def handle_if_end(self):
        """
        Handles the end of an if block
        """
        self._write_comment('====== End If ======')

        if_context = self.if_labels[-1]
        self.templates.emit_label(if_context.end)
        self.if_labels.pop()

    def handle_while(self, cond):
        """
        Handles the start of a while loop.
        """
        self._write_comment('====== While ======')
        self._write_comment('  Condition: {}', cond)

        while_context = WhileLabels(
                next(self.label_maker), 
                next(self.label_maker))
        self.while_labels.append(while_context)

        tmp_reg = self.templates.tmp_regs[0]

        self.templates.emit_label(while_context.cond)

        temp_context = self.current_context.func_stack.get_temp_context(self)
        with temp_context:
            cond_dest, cond_type = (
                self._compile_expression(cond, temp_context))

            if cond_type is not types.Integer:
                self.error(*cond.loc, 'Conditional must be an integer')

            self.templates.emit_load_stack_word(tmp_reg, cond_dest)

        # The position outside the context is deliberate - we have to avoid
        # any situations where the code doesn't execute the stack adjustment
        # code. In this case, indenting this write call will leave the stack
        # deeper than it should be, if the branch is taken
        self.templates.emit_branch_if_zero(tmp_reg, while_context.exit)

    def handle_while_end(self):
        """
        Handles the end of a while loop.
        """
        self._write_comment('====== While End ======')

        while_context = self.while_labels[-1]
        self.templates.emit_jump(while_context.cond)
        self.templates.emit_label(while_context.exit)

    def handle_break(self):
        """
        Handles breaking out of a while loop.
        """
        self._write_comment('==== Break ====')
        try:
            while_context = self.while_labels[-1]
        except IndexError:
            self.error(self.line, self.col, 'Cannot have break outside of while loop')

        self.templates.emit_jump(while_context.exit)

    def handle_continue(self):
        """
        Handles continuing at the top of a while loop.
        """
        self._write_comment('==== Continue  ====')
        try:
            while_context = self.while_labels[-1]
        except IndexError:
            self.error(self.line, self.col, 'Cannot have break outside of while loop')

        self.templates.emit_jump(while_context.cond)

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
                self.error(*expr.loc, 'Returned expression must be the same as the function return type')

            # Since we're barred from returning structures, the most we'll
            # have to deal with is words
            ret_type_size = self._type_size(ret_type)

            if ret_type_size == 1:
                self.templates.emit_load_stack_byte(self.templates.return_reg, ret_dest)
            elif ret_type_size == 4:
                self.templates.emit_load_stack_word(self.templates.return_reg, ret_dest)

        self.templates.emit_jump(self.func_exit_label)

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
