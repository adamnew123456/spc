"""
A common backend for 32-bit processors.
"""
import logging

from ..backend import BaseBackend
from ..backend_utils import (
    CoercionContext, comment_after, ContextMixin, IfLabels, SwitchLabels,
    ThirtyTwoMixin, WhileLabels,
)
from .. import expressions
from ..require_processor import RequireProcessor
from ..symbols import SymbolTable
from .. import types
from ..util import (
    make_label_maker, mangle_label, unescape_bytes
)

LOGGER = logging.getLogger('spc.common32')

class Common32Backend(ContextMixin, ThirtyTwoMixin, BaseBackend):
    """
    Emits MIPS assembly code compatible with the MARS simulator.
    """
    def __init__(self, templates, output, filename, is_library):
        BaseBackend.__init__(self, output, filename, is_library)
        ContextMixin.__init__(self)

        self.file_namespace = None
        self.templates = templates
        self.undefined_funcs = set()
        self.comment_fmt = templates.comment_fmt
        self.label_maker = make_label_maker()

        # It's important for the templates to get to the backend, so they can
        # actually spit out code
        self.templates.backend = self

    def _platform(self):
        """
        Returns the (OS, architecture) pair of the template.
        """
        return self.templates.platform_os, self.templates.platform_arch

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
        if self.file_namespace is None:
            self.error(self.line, self.col,
                'Must have (namespace ...) declaration before toplevel declare')

        was_type_name = isinstance(decl_type, types.TypeName)
        decl_type = self._resolve_if_type_name(decl_type)

        if isinstance(decl_type, types.StringLiteral):
            self._write_comment('  Declaring string {}', name)

            escaped = unescape_bytes(decl_type.bytes).encode('ascii')
            self.ctx_values[name] = types.PointerTo(types.Byte)
            self.ctx_values.meta_set(name, 'visible', {self.ctx_namespace})
            self.ctx_values.meta_set(name, 'array', len(escaped))

            self.verify_context.add_value(name)

            if self.in_function:
                # We can't really use .asciiz in a function, so we'll have to
                # make do with copying bytes manually
                escaped += b'\0'
                self.ctx_stack.add_local(name, len(escaped), 1)
                base_addr = self.ctx_stack.local_offset

                self.templates.emit_stack_string(base_addr, escaped)
            else:
                self.ctx_values.meta_set(name, 'global', True)

                self.templates.emit_label(mangle_label(name))
                self.templates.emit_static_string(decl_type.bytes.decode('ascii'))
        elif was_type_name or isinstance(decl_type, types.RAW_TYPES):
            self._write_comment('  Declaring variable {} :: {}', name, decl_type)

            was_array = isinstance(decl_type, types.ArrayOf)
            self.ctx_values[name] = types.decay_if_array(decl_type)
            self.ctx_values.meta_set(name, 'visible', {self.ctx_namespace})

            self.verify_context.add_value(name)

            if self.in_function:
                # Raw types have to be allocated stack space
                size = self._type_size(decl_type)
                alignment = self._type_alignment(decl_type)
                self.ctx_stack.add_local(name, size, alignment)
            else:
                # Raw types have to be allocated space inside of .data
                # and given a label
                self._write_comment('  As a global')

                self.ctx_values.meta_set(name, 'global', True)

                size = self._type_size(decl_type)
                self.templates.emit_label(mangle_label(name))
                self.templates.emit_static_space(size)

            if was_array:
                self.ctx_values.meta_set(name, 'array', True)

        elif isinstance(decl_type, types.Struct):
            # Structure types are treated as structure definitions, which 
            # bind a type definition
            self._write_comment('  Declaring structure {} :: {}', name, decl_type)
            self.ctx_types[name] = decl_type
            self.ctx_types.meta_set(name, 'visible', {self.ctx_namespace})

            self.verify_context.add_type(name)
        elif isinstance(decl_type, types.FunctionDecl):
            # Function declarations signify a function that is defined 
            # somewhere
            if self.in_function:
                self.error(self.line, self.col, 'Cannot declare nested functions')

            self.undefined_funcs.add(name)

            self._write_comment('  Declaring function {} :: {}', name, decl_type)

            full_decl_type = symbols.namespace_func_decl(decl_type, self.ctx_namespace)

            self.ctx_values[name] = full_decl_type
            self.ctx_values.meta_set(name, 'global', True)
            self.ctx_values.meta_set(name, 'visible', {self.ctx_namespace})

            self.verify_context.add_value(name)

            if name == 'main':
                full_name = symbols.join_namespace(self.ctx_namespace, name)
                self.main_function = full_name
        elif isinstance(decl_type, types.AliasDef):
            # Alias definitions bind an existing type to a new name
            self._write_comment('  Declaring alias {} => {}', name, decl_type.type)
            self.ctx_types[name] = decl_type.type
            self.ctx_types.meta_set(name, 'visible', {self.ctx_namespace})

            self.verify_context.add_type(name)

    def handle_decl_block_end(self):
        """
        Handles the end of a declaration block.

        At the top level, this starts the .text section and writes a jump to
        main. In a function, this adds enough space to the stack frame for
        the local variables.
        """
        # Make sure that all the types we just defined don't reference
        # any types that don't exist, or do any other nasty things
        self._verify_types()

        if self.in_function:
            self.ctx_stack.expand_locals()
        else:
            self.templates.emit_text_segment()
            if not self.library:
                self.templates.emit_prog_header()
    
    def handle_namespace(self, namespace):
        """
        Sets the file's global namespace.
        """
        if self.file_namespace is not None:
            self.error(self.line, self.col,
                'Cannot have duplicate (namespace ...) declarations')

        self.file_namespace = namespace
        self._register_file_ns(self.file_namespace)

    def handle_require(self, filename):
        """
        Handles a require which loads the given filename.
        """
        if self.in_function:
            self.error(self.line, self.col,
                "Cannot load another file inside of a function")

        if self.file_namespace is None:
            self.error(self.line, self.col,
                'Must have (namespace ...) declaration before (require ...)')

        try:
            processor = RequireProcessor.require(filename, self)
            if processor is None:
                return

            for type_name in processor.exported_types:
                self._write_comment('Importing definition: {}', type_name)
                self.ctx_types.meta_get(type_name, 'visible').add(self.ctx_namespace)

            for val_name in processor.exported_values:
                self._write_comment('Importing value: {}', val_name)
                self.ctx_values.meta_get(val_name, 'visible').add(self.ctx_namespace)
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

        if self.file_namespace is None:
            self.error(self.line, self.col,
                'Must have (namespace ...) declaration before (export ...)')

        for name in names:
            if name[0] not in "'*":
                self.error(self.line, self.col,
                    "Export must begin with either ' or *")

            if name[0] == "'":
                full_name = self.ctx_values.resolve(name[1:]) 
            else:
                full_name = self.ctx_types.resolve(name[1:])

            owning_ns, _ = symbols.split_namespace(full_name)
            if owning_ns != self.file_namespace:
                self.error(self.line, self.col,
                    'Cannot export foreign type or value "{}" (owned by "{}")', full_name, owning_ns)

            # Types don't actually exist for the assembler, so no code has to e
            # generated for them
            if name[0] != "'":
                continue

            name = name[1:]

            try:
                type_of = self.ctx_values[name]
            except KeyError:
                self.error(self.line, self.col,
                    'Undefined export "{}"', name)

            if not types.can_be_global(type_of):
                self.error(self.line, self.col,
                    'Cannot export value of type "{}"', type_of)

            full_name = symbols.join_namespace(self.ctx_namespace, name)
            self.templates.emit_export(full_name, type_of)

    def handle_func_def_start(self, name, params):
        """
        Handles the beginning of a function.
        """
        self.in_function = True
        self.func_exit_label = next(self.label_maker)

        # This has to be done before we push the context, since the function
        # "belongs" to this context and not its local context
        full_name = symbols.join_namespace(self.ctx_namespace, name)

        try:
            self.undefined_funcs.remove(name)
            func_defn = self.ctx_values[name]
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
                self.ctx_stack.pad_param(4 - (alignment % 4))
                first_arg = False

            self.ctx_stack.add_param(param, type_size, alignment)
            self.ctx_values[param] = param_type
            self.ctx_values.meta_set(param, 'visible', {self.ctx_namespace})

        if last_alignment is not None and last_alignment % 4 != 0:
            self._write_comment('Tail Padding {} bytes', 4 - (last_alignment % 4))
            self.ctx_stack.pad_param(4 - (last_alignment % 4))

        self.func_ret_type = self._resolve_if_type_name(func_defn.return_type)

        self.templates.emit_label(mangle_label(full_name))
        self.templates.emit_func_header(full_name)

    def handle_func_def_end(self):
        """
        Handles the end of the function definition.

        Writes out the end label and the code for doing a function return.
        """
        self.templates.emit_label(self.func_exit_label)
        self.ctx_stack.cleanup_locals()
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
            func_defn = self.ctx_values[name]
        except KeyError as exn:
            self.error(self.line, self.col, 'Undefined function "{}"', name)

        if not isinstance(func_defn, types.FunctionDecl):
            self.error(self.line, self.col, 'Value {} is not a function', name)

        full_name = symbols.join_namespace(self.ctx_namespace, name)
        self.templates.emit_label(mangle_label(full_name))
        self.templates.emit_asm(full_name, code)

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

        # by_ref is useful to have - consider the following example with structs:
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

        coercer = CoercionContext(self, temp_context, self.templates)

        if isinstance(expr, expressions.Variable):
            # by_ref obviously makes sense - otherwise, assignment to a variable
            # couldn't work at all!
            real_name = expr.name

            try:
                type_of = self.ctx_values[real_name]
            except KeyError:
                self.error(*expr.loc, 'Variable "{}" not defined', real_name)

            if isinstance(type_of, types.TypeName):
                type_of = self._resolve_if_type_name(type_of)

            if isinstance(type_of, types.FunctionDecl):
                if by_ref:
                    # You can't logically assign to the address you get back, so
                    # it doesn't make sense to even try
                    self.error(*expr.loc, 'Cannot use function in a ref context')

                real_name = type_of.name
                type_of = types.func_decl_to_ptr(type_of)

                # Set the flag anyway, since trying to load a function 
                # 'by value' would load code, which doesn't make sense
                self._write_comment('  Variable load: function pointer {}', real_name)
                by_ref = True

            is_array = self.ctx_values.meta_get(real_name, 'array', False)
            if isinstance(type_of, types.PointerTo) and is_array:
                # Arrays are promoted to by_ref, but for a special reason - the
                # want to be pointers (thus a non-by-ref load should get back
                # the address) but are more like values. Making them by_ref
                # allows them to be used like pointers, but with the side
                # effect that they can't be assigned to
                if by_ref:
                    self.error(*expr.loc, 'Cannot use array in a ref context')

                self._write_comment('  Variable load: array {}', expr.name)
                by_ref = True

            if not self.ctx_values.is_visible(real_name):
                self.error(*expr.loc, 
                    'Variable "{}" is not visible in current scope', real_name)

            if by_ref:
                type_size = self._type_size(types.Integer)
                type_align = self._type_alignment(types.Integer)
            else:
                type_size = self._type_size(type_of)
                type_align = self._type_alignment(type_of)

            dest_offset = temp_context.add_temp(type_size, type_align)
            tmp_reg = self.templates.tmp_regs[0]

            if self.ctx_values.meta_get(real_name, 'global', False):
                # Global variables are somewhere in .data land, labeled by their name
                self.templates.emit_load_static_addr(tmp_reg, real_name)
            else:
                # Local variables are on the stack
                stack_offset = self.ctx_stack[real_name]
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
        elif isinstance(expr, expressions.Char):
            if by_ref:
                # Invalid - you can't assign to a character
                self.error(*expr.loc, 'Cannot use an integer literal in a ref context')

            dest_offset = temp_context.add_temp(self._type_size(types.Byte),
                                                self._type_alignment(types.Byte))

            tmp_reg = self.templates.tmp_regs[0]
            self.templates.emit_load_byte(tmp_reg, expr.character.encode('ascii')[0])
            self.templates.emit_save_stack_byte(tmp_reg, dest_offset)

            return dest_offset, types.Byte
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
        elif isinstance(expr, expressions.Cast):
            # by_ref doesn't work, again because this can't be assigned to directly
            if by_ref:
                self.error(*expr.loc, '(cast t x) cannot be used in a ref context')

            expr_dest, expr_type = self._compile_expression(expr.expr, temp_context)

            # Cast has a couple of valid scenarios - for types A, B:
            #
            # 1. Casting from A to A
            # 2. Casting from (pointer-to A) to (pointer-to B)
            # 3. Casting from integer to (pointer-to A)
            #  3a. Casting from byte to (pointer-to A)
            # 4. Casting from (pointer-to A) to integer
            # 5. Casting from byte to integer
            # 6. Casting from integer to byte
            #
            # Note that there is no corresponding 4a, since the result will be
            # coerced to a byte by the containing expression.
            ret_type = self._resolve_if_type_name(expr.type)

            try:
                # This takes care of cases 1, 5 and 6
                return coercer.coerce(expr_dest, expr_type, ret_type)
            except TypeError:
                if isinstance(expr_type, types.PointerTo) and isinstance(ret_type, types.PointerTo):
                    return expr_dest, ret_type
                elif expr_type is types.Integer and isinstance(ret_type, types.PointerTo):
                    return expr_dest, ret_type
                elif expr_type is types.Byte and isinstance(ret_type, types.PointerTo):
                    int_dest, _ = coercer.coerce(expr_dest, expr_type, types.Integer)
                    return int_dest, ret_type
                elif isinstance(expr_type, types.PointerTo) and ret_type is types.Integer:
                    return expr_dest, ret_type
                else:
                    self.error(*expr.loc, 'Cannot cast {} to {}', expr_type, ret_type)
        elif isinstance(expr, expressions.Array):
            # by_ref works, since arrays can be assigned to
            array_dest, array_type = self._compile_expression(expr.array, temp_context)

            if not isinstance(array_type, types.PointerTo):
                self.error(*expr.loc, '(array x i) requires x to be a pointer type')

            _index_dest, _index_type = self._compile_expression(expr.index, temp_context)
            try:
                index_dest, index_type = coercer.coerce(_index_dest, _index_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)
    
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

            _lhs_dest, _lhs_type = self._compile_expression(expr.lhs, temp_context)
            _rhs_dest, _rhs_type = self._compile_expression(expr.rhs, temp_context)
            is_ptr_op = isinstance(_lhs_type, types.PointerTo)
                
            if is_ptr_op:
                if expr.kind not in (expressions.ARITH_PLUS, expressions.ARITH_MINUS):
                    self.error(*expr.loc,
                        'Pointer arithmetic must be + or minus')

                lhs_dest, lhs_type = _lhs_dest, _lhs_type
            else:
                try:
                    lhs_dest, lhs_type = coercer.coerce(_lhs_dest, _lhs_type, types.Integer)
                except TypeError as err:
                    self.error(*expr.loc, '{}', err)

            try:
                rhs_dest, rhs_type = coercer.coerce(_rhs_dest, _rhs_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)
    
            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            lhs_reg = self.templates.tmp_regs[0]
            tmp_reg = self.templates.tmp_regs[1]

            if not is_ptr_op:
                self.templates.emit_load_stack_word(lhs_reg, lhs_dest)

            self.templates.emit_load_stack_word(tmp_reg, rhs_dest)

            if expr.kind == expressions.ARITH_PLUS:
                if is_ptr_op:
                    _, element_size = types.elem_size_with_padding(self, lhs_type)
                    self.templates.emit_array_offset(lhs_reg, lhs_dest, rhs_dest, element_size)
                else:
                    self.templates.emit_add(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_MINUS:
                if is_ptr_op:
                    _, element_size = types.elem_size_with_padding(self, lhs_type)
                    self.templates.emit_left_array_offset(lhs_reg, lhs_dest, rhs_dest, element_size)
                else:
                    self.templates.emit_sub(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_TIMES:
                self.templates.emit_mul(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_DIVIDE:
                self.templates.emit_div(lhs_reg, tmp_reg)
            elif expr.kind == expressions.ARITH_MOD:
                self.templates.emit_mod(lhs_reg, tmp_reg)

            self.templates.emit_save_stack_word(lhs_reg, dest_offset)

            if is_ptr_op:
                return dest_offset, lhs_type
            else:
                return dest_offset, types.Integer
        elif isinstance(expr, expressions.Compare):
            if by_ref:
                self.error(*expr.loc, 'Cannot use & result in a ref context')

            _lhs_dest, _lhs_type = self._compile_expression(expr.lhs, temp_context)
            _rhs_dest, _rhs_type = self._compile_expression(expr.rhs, temp_context)
            
            if isinstance(_lhs_type, types.PointerTo):
                lhs_dest, lhs_type = _lhs_dest, _lhs_type
            else:
                try:
                    lhs_dest, lhs_type = coercer.coerce(_lhs_dest, _lhs_type, types.Integer)
                except TypeError as err:
                    self.error(*expr.loc, '{}', err)

            if isinstance(_rhs_type, types.PointerTo):
                rhs_dest, rhs_type = _rhs_dest, _rhs_type
            else:
                try:
                    rhs_dest, rhs_type = coercer.coerce(_rhs_dest, _rhs_type, types.Integer)
                except TypeError as err:
                    self.error(*expr.loc, '{}', err)

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

            _lhs_dest, _lhs_type = self._compile_expression(expr.lhs, temp_context)
            _rhs_dest, _rhs_type = self._compile_expression(expr.rhs, temp_context)

            try:
                lhs_dest, lhs_type = coercer.coerce(_lhs_dest, _lhs_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)

            try:
                rhs_dest, rhs_type = coercer.coerce(_rhs_dest, _rhs_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)

            int_size = self._type_size(types.Integer)
            int_align = self._type_alignment(types.Integer)
            dest_offset = temp_context.add_temp(int_size, int_align)

            lhs_reg = self.templates.tmp_regs[0]
            tmp_reg = self.templates.tmp_regs[1]

            self.templates.emit_load_stack_word(lhs_reg, lhs_dest)
            self.templates.emit_load_stack_word(tmp_reg, rhs_dest)
            
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

            _expr_dest, _expr_type = self._compile_expression(expr.expr, temp_context)
            try:
                expr_dest, expr_type = coercer.coerce(_expr_dest, _expr_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)

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

            _lhs_dest, _lhs_type = self._compile_expression(expr.lhs, temp_context)
            try:
                lhs_dest, lhs_type = coercer.coerce(_lhs_dest, _lhs_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)
            
            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, lhs_dest)
            self.templates.emit_branch_if_zero(tmp_reg, end_label)

            # The reason for the sub-context here is that there is an extra stack
            # allocation in the case where the expression doesn't short circuit,
            # and we don't want the stack to become inconsistent across the branch
            sub_context = temp_context.get_temp_context()
            with sub_context:
                _rhs_dest, _rhs_type = self._compile_expression(expr.rhs, sub_context)

                sub_coercer = coercer.copy_with_context(sub_context)
                try:
                    rhs_dest, rhs_type = sub_coercer.coerce(_rhs_dest, _rhs_type, types.Integer)
                except TypeError as err:
                    self.error(*expr.loc, '{}', err)

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

            _lhs_dest, _lhs_type = self._compile_expression(expr.lhs, temp_context)
            try:
                lhs_dest, lhs_type = coercer.coerce(_lhs_dest, _lhs_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)

            tmp_reg = self.templates.tmp_regs[0]

            self.templates.emit_load_stack_word(tmp_reg, lhs_dest)
            self.templates.emit_branch_if_nonzero(tmp_reg, end_label)

            sub_context = temp_context.get_temp_context()
            with sub_context:
                _rhs_dest, _rhs_type = self._compile_expression(expr.rhs, sub_context)

                sub_coercer = coercer.copy_with_context(sub_context)
                try:
                    rhs_dest, rhs_type = sub_coercer.coerce(_rhs_dest, _rhs_type, types.Integer)
                except TypeError as err:
                    self.error(*expr.loc, '{}', err)

                self.templates.emit_load_stack_word(tmp_reg, rhs_dest)

            self.templates.emit_label(end_label)
            self.templates.emit_save_stack_word(tmp_reg, dest_offset)

            return dest_offset, types.Integer
        elif isinstance(expr, expressions.Not):
            if by_ref:
                self.error(*expr.loc, 'Cannot use logical result in a ref context')

            _expr_dest, _expr_type = (
                self._compile_expression(expr.expr, temp_context))

            try:
                expr_dest, expr_type = coercer.coerce(_expr_dest, _expr_type, types.Integer)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)

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
                _param_dest, _param_real_type = self._compile_expression(param, temp_context)
                try:
                    param_dest, param_real_type = coercer.coerce(_param_dest, 
                            _param_real_type, param_expected_type)
                except TypeError as err:
                    self.error(*expr.loc, '{}', err)

                rev_param_dests.append(param_dest)
                rev_param_types.append(param_expected_type)

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

        temp_context = self.ctx_stack.get_temp_context(self)
        coercer = CoercionContext(self, temp_context, self.templates)

        with temp_context:
            assign_dest, assign_type = (
                self._compile_expression(assignable, temp_context, by_ref=True))

            _value_dest, _value_type = (
                self._compile_expression(expression, temp_context))

            try:
                value_dest, value_type = coercer.coerce(_value_dest, 
                        _value_type, assign_type)
            except TypeError as err:
                self.error(self.line, self.col, '{}', err)

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

        temp_context = self.ctx_stack.get_temp_context(self)
        coercer = CoercionContext(self, temp_context, self.templates)

        with temp_context:
            _cond_dest, _cond_type = (
                self._compile_expression(cond, temp_context))

            try:
                cond_dest, cond_type = coercer.coerce(_cond_dest, _cond_type, types.Integer)
            except TypeError as err:
                self.error(self.line, self.col, '{}', err)

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

    def handle_switch_start(self):
        """
        Handles the start of switch block.
        """
        self._write_comment('===== Switch Start =====')

        switch_context = SwitchLabels(next(self.label_maker))
        self.switch_labels.append(switch_context)

    def handle_case_start(self, cond):
        """
        Handles the start of a case block.
        """
        self._write_comment('===== Case Start =====')
        self._write_comment('Condition: {}', cond)

        switch_context = self.switch_labels[-1]
        switch_context.case_end_label = next(self.label_maker)

        if cond is not None:
            tmp_reg = self.templates.tmp_regs[0]
            temp_context = self.ctx_stack.get_temp_context(self)
            coercer = CoercionContext(self, temp_context, self.templates)

            with temp_context:
                _cond_dest, _cond_type = (
                    self._compile_expression(cond, temp_context))

                try:
                    cond_dest, cond_type = coercer.coerce(_cond_dest, _cond_type, types.Integer)
                except TypeError as err:
                    self.error(self.line, self.col, '{}', err)

                self.templates.emit_load_stack_word(tmp_reg, cond_dest)

            # The position outside the context is deliberate - we have to avoid
            # any situations where the code doesn't execute the stack adjustment
            # code. In this case, indenting this write call will leave the stack
            # deeper than it should be, if the branch is taken
            self.templates.emit_branch_if_zero(tmp_reg, switch_context.case_end_label)

    def handle_case_end(self):
        """
        Handles the end of a case block.
        """
        self._write_comment('===== Case End =====')
        switch_context = self.switch_labels[-1]
        
        self.templates.emit_jump(switch_context.end_label)
        self.templates.emit_label(switch_context.case_end_label)

    def handle_switch_end(self):
        """
        Handles the end of a switch block.
        """
        self._write_comment('===== Switch End =====')
        switch_context = self.switch_labels.pop()
        self.templates.emit_label(switch_context.end_label)

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

        temp_context = self.ctx_stack.get_temp_context(self)
        coercer = CoercionContext(self, temp_context, self.templates)

        with temp_context:
            _cond_dest, _cond_type = (
                self._compile_expression(cond, temp_context))

            try:
                cond_dest, cond_type = coercer.coerce(_cond_dest, _cond_type, types.Integer)
            except TypeError as err:
                self.error(self.line, self.col, '{}', err)

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

        temp_context = self.ctx_stack.get_temp_context(self)
        coercer = CoercionContext(self, temp_context, self.templates)

        with temp_context:
            _ret_dest, _ret_type = (
                self._compile_expression(expr, temp_context))

            try:
                ret_dest, ret_type = coercer.coerce(_ret_dest, _ret_type, self.func_ret_type)
            except TypeError as err:
                self.error(*expr.loc, '{}', err)

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

        temp_context = self.ctx_stack.get_temp_context(self)
        with temp_context:
            self._compile_expression(expr, temp_context)
