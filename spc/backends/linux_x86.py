"""
Linux compiler backend - responsible for taking program events emitted by the
driver and converting them into code.
"""
import logging

from ..backend_utils import FunctionStack
from .common32 import Common32Backend
from ..util import mangle_label

LOGGER = logging.getLogger('spc.linux_x86')

class LinuxX86Stack(FunctionStack):
    """
    Specializes the default function stack for the x86 Linux code 
    generator.
    """

    #
    # Stack layout:
    # 
    #   | ...     |
    #   | param_2 |
    #   | param_1 |__/-- 8(%ebp)
    #   | <ret>   |__/-- 4(%ebp)
    #   | old_ebp |__/-- %ebp
    #   | local_1 |
    #   | local_2 |
    #   | ...     |
    #

    def _starting_locals_offset(self):
        return 0

    def _starting_param_offset(self):
        return 8

    def _expand_stack(self, size):
        self.backend._write_instr('    sub ${}, %esp', size)

    def _shrink_stack(self, size):
        self.backend._write_instr('    add ${}, %esp', size)

class LinuxX86Templates:
    """
    Template code which is plugged into the generic 32-bit backend, which
    generates GAS-compatible x86 code.
    """
    frame_reg = '%ebp'
    return_reg = '%eax'
    tmp_regs = ['%eax', '%edx']
    comment_fmt = '/* {} */'

    def __init__(self):
        self.backend = None

    def make_func_stack(self):
        return LinuxX86Stack(self.backend)

    def _write_instr(self, *args, **kwargs):
        """
        Convenience method to avoid writing out 'backend'
        """
        self.backend._write_instr(*args, **kwargs)

    def emit_label(self, label):
        self._write_instr('{}:', label)

    def emit_data_segment(self):
        self._write_instr('.data')

    def emit_stack_string(self, base_addr, escaped_bytes):
        for idx, byte in enumerate(escaped_bytes):
            self._write_instr('    movb ${}, %al', byte)
            self._write_instr('    movb %al, {}(%ebp)', base_addr + idx)

    def emit_static_string(self, unescaped_bytes):
        self._write_instr('    .string "{}"', unescaped_bytes)

    def emit_static_space(self, size):
        self._write_instr('    .space {}', size)

    def emit_text_segment(self):
        self._write_instr('.text')

    def emit_prog_header(self):
        self._write_instr('.global _start')
        self._write_instr('_start:')
        self._write_instr('    call {}', mangle_label('main'))
        self._write_instr('    movl $1, %eax')
        self._write_instr('    movl $0, %ebx')
        self._write_instr('    int $0x80')

    def emit_export(self, name, type_of):
        self._write_instr('.globl {}', mangle_label(name))

    def emit_func_header(self, name):
        self._write_instr('    pushl %ebp')
        self._write_instr('    movl %esp, %ebp')

    def emit_func_footer(self):
        self._write_instr('    popl %ebp')
        self._write_instr('    ret')

    def emit_asm(self, name, code):
        for instr in code.splitlines():
            self._write_instr('{}', instr)

    def emit_move_word(self, move_reg, 
                        src_reg, src_offset,
                        dest_reg, dest_offset):
        self._write_instr('    movl {}({}), {}', src_offset, src_reg, move_reg)
        self._write_instr('    movl {}, {}({})', move_reg, dest_offset, dest_reg)

    def emit_move_byte(self, move_reg,
                        src_reg, src_offset,
                        dest_reg, dest_offset):
        self._write_instr('    movb {}({}), {}', src_offset, src_reg, move_reg)
        self._write_instr('    movb {}, {}({})', move_reg, dest_offset, dest_reg)

    def emit_load_int(self, reg, value):
        self._write_instr('    movl ${}, {}', value, reg)

    def emit_load_static_addr(self, reg, name):
        self._write_instr('    movl ${}, {}', mangle_label(name), reg)

    def emit_load_stack_addr(self, reg, offset):
        self._write_instr('    movl %ebp, {}', reg)
        self._write_instr('    add ${}, {}', offset, reg)

    def emit_load_stack_word(self, reg, offset):
        self._write_instr('    movl {}(%ebp), {}', offset, reg)

    def emit_save_stack_word(self, reg, offset):
        self._write_instr('    movl {}, {}(%ebp)', reg, offset)

    def emit_load_stack_byte(self, reg, offset):
        self._write_instr('    movb {}(%ebp), {}', offset, reg)

    def emit_save_stack_byte(self, reg, offset):
        self._write_instr('    movb {}, {}(%ebp)', reg, offset)

    def emit_int_to_byte(self, reg):
        self._write_instr('    shll $24, {}', reg)
        self._write_instr('    sarl $24, {}', reg)

    def emit_byte_to_int(self, reg):
        # This is necessary because movb uses the byte registers, unlike
        # MARS which zeros a word register and then loads the byte. The byte
        # registers leave the other 24-bits intact, which we don't want
        # because the byte we're upcasting should have zeroes in those bits
        self._write_instr('    shll $24, {}', reg)
        self._write_instr('    sarl $24, {}', reg)

    def emit_array_offset(self, reg, array_offset, index_offset, elem_size):
        self._write_instr('    movl {}(%ebp), %eax', index_offset, reg)
        self._write_instr('    imull ${}, %eax, %ecx', elem_size)
        self._write_instr('    movl {}(%ebp), {}', array_offset, reg)
        self._write_instr('    addl %ecx, {}', reg)

    def emit_struct_offset(self, reg, total_offset):
        self._write_instr('    add ${}, {}', total_offset, reg)

    def emit_add(self, dest_reg, rhs_reg):
        self._write_instr('    add {}, {}', rhs_reg, dest_reg)

    def emit_sub(self, dest_reg, rhs_reg):
        self._write_instr('    sub {}, {}', rhs_reg, dest_reg)

    def emit_mul(self, dest_reg, rhs_reg):
        self._write_instr('    imul {}, {}', rhs_reg, dest_reg)
        if dest_reg != '%eax':
            self._write_instr('    movl %eax, {}', dest_reg)

    # These are a bit bizarre - in x86, to do the division:
    #
    # c = a / b
    #
    # You have to load a into EDX:EAX (EDX are bits 63-32,
    # EAX are bits 31-0 of this weird 64-bit integer made of
    # those two registers mashed together). This typically
    # involves zeroing EDX, moving a into EAX, and then
    # sign extending EAX into EDX (via the instruction CLTD).
    #
    # Then, you divide this implicit argument by b, and the
    # processor will dump the result into EAX and the remainder
    # into EDX

    def emit_div(self, dest_reg, rhs_reg):
        move_rhs = rhs_reg in ('%eax', '%edx')

        if move_rhs:
            self._write_instr('    movl {}, %ecx', rhs_reg)

        self._write_instr('    xor %edx, %edx')
        self._write_instr('    cltd')

        if move_rhs:
            self._write_instr('    idivl %ecx')
        else:
            self._write_instr('    idivl {}', rhs_reg)

        if dest_reg != '%eax':
            self._write_instr('    movl %eax, {}', dest_reg)

    def emit_mod(self, dest_reg, rhs_reg):
        move_rhs = rhs_reg in ('%eax', '%edx')

        if move_rhs:
            self._write_instr('    movl {}, %ecx', rhs_reg)

        self._write_instr('    xor %edx, %edx')
        self._write_instr('    cltd')

        if move_rhs:
            self._write_instr('    idivl %ecx')
        else:
            self._write_instr('    idivl {}', rhs_reg)

        if dest_reg != '%edx':
            self._write_instr('    movl %edx, {}', dest_reg)

    class CompareContext:
        def __init__(self, owner, lhs, rhs):
            self.owner = owner
            self.lhs = lhs
            self.rhs = rhs
            self.skip_label = next(owner.backend.label_maker)

        def __enter__(self):
            self.owner._write_instr('    cmpl {}, {}', self.rhs, self.lhs)
            self.owner._write_instr('    movl $1, {}', self.lhs)
            return self

        def __exit__(self, *err):
            self.owner._write_instr('    xor {}, {}', self.lhs, self.lhs)
            self.owner._write_instr('{}:', self.skip_label)

    def emit_less(self, dest_reg, rhs_reg):
        with self.CompareContext(self, dest_reg, rhs_reg) as ctx:
            self._write_instr('    jl {}', ctx.skip_label)

    def emit_greater(self, dest_reg, rhs_reg):
        with self.CompareContext(self, dest_reg, rhs_reg) as ctx:
            self._write_instr('    jg {}', ctx.skip_label)

    def emit_lesseq(self, dest_reg, rhs_reg):
        with self.CompareContext(self, dest_reg, rhs_reg) as ctx:
            self._write_instr('    jle {}', ctx.skip_label)

    def emit_greateq(self, dest_reg, rhs_reg):
        with self.CompareContext(self, dest_reg, rhs_reg) as ctx:
            self._write_instr('    jge {}', ctx.skip_label)

    def emit_eq(self, dest_reg, rhs_reg):
        with self.CompareContext(self, dest_reg, rhs_reg) as ctx:
            self._write_instr('    je {}', ctx.skip_label)

    def emit_noteq(self, dest_reg, rhs_reg):
        with self.CompareContext(self, dest_reg, rhs_reg) as ctx:
            self._write_instr('    jne {}', ctx.skip_label)

    def emit_bit_and(self, dest_reg, rhs_reg):
        self._write_instr('    andl {}, {}', rhs_reg, dest_reg)

    def emit_bit_or(self, dest_reg, rhs_reg):
        self._write_instr('    orl {}, {}', rhs_reg, dest_reg)

    def emit_shiftleft(self, dest_reg, rhs_reg):
        self._write_instr('    shll {}, {}', rhs_reg, dest_reg)

    def emit_shiftright_arith(self, dest_reg, rhs_reg):
        self._write_instr('    shrl {}, {}', rhs_reg, dest_reg)

    def emit_shiftright_log(self, dest_reg, rhs_reg):
        self._write_instr('    sarl {}, {}', rhs_reg, dest_reg)

    def emit_bit_not(self, reg):
        self._write_instr('    not {}', reg)

    def emit_branch_if_zero(self, reg, label):
        self._write_instr('    cmp $0, {}', reg)
        self._write_instr('    jz {}', label)

    def emit_branch_if_nonzero(self, reg, label):
        self._write_instr('    cmp $0, {}', reg)
        self._write_instr('    jnz {}', label)

    def emit_not(self, reg):
        skip_label = next(self.backend.label_maker)

        self._write_instr('    cmpl $0, {}', reg)

        # Not xor, since that switches with flags, which we need to preserve
        # in order to do the jump
        self._write_instr('    movl $0, {}', reg)

        self._write_instr('    jnz {}', skip_label)
        self._write_instr('    movl $1, {}', reg)
        self._write_instr('{}:', skip_label)

    def emit_indirect_call(self, reg):
        self._write_instr('    call *{}', reg)

    def emit_jump(self, label):
        self._write_instr('    jmp {}', label)

def get_backend(output, filename, is_library):
    """
    Returns the backend represented by this module.
    """
    return Common32Backend(LinuxX86Templates(), output, filename, is_library)
