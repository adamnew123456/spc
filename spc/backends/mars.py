"""
MARS compiler backend - responsible for taking program events emitted by the
driver and converting them into code.
"""
import logging

from ..backend_utils import FunctionStack
from .common32 import Common32Backend
from .. import types
from ..util import mangle_label

LOGGER = logging.getLogger('spc.mars')

class MarsFunctionStack(FunctionStack):
    """
    Specializes the default function stack for the MARS code generator.
    """

    #
    # Stack layout:
    #
    #   |  ...    |
    #   | param_2 |
    #   | param_1 |__/-- $fp
    #   | old_fp  |__/-- -4($fp)
    #   | old_ra  |__/-- -8($fp)
    #   | local_1 |
    #   | local_2 |
    #   |  ...    |
    #

    def _starting_locals_offset(self):
        return -8

    def _starting_param_offset(self):
        return 0

    def _expand_stack(self, size):
        self.backend._write_instr('    addi $sp, $sp, -{}', size)

    def _shrink_stack(self, size):
        self.backend._write_instr('    addi $sp, $sp, {}', size)

class MarsTemplates:
    """
    Template code which is plugged into the generic 32-bit backend, which
    generates MARS code.
    """
    frame_reg = '$fp'
    return_reg = '$v0'

    # Right now, the common32 backend only ever asks for two of these, so
    # this is acceptable
    tmp_regs = ['$t0', '$t1']

    platform_name = 'mars'
    comment_fmt = '# {}'

    def __init__(self):
        self.backend = None

    def make_func_stack(self):
        return MarsFunctionStack(self.backend)

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
            self._write_instr('    li $t0, {}', byte)
            self._write_instr('    sb $t0, {}($fp)', base_addr + idx)

    def emit_static_string(self, unescaped_bytes):
        self._write_instr('    .asciiz "{}"', unescaped_bytes)

    def emit_static_space(self, size):
        self._write_instr('    .space {}', size)

    def emit_text_segment(self):
        self._write_instr('.text')

    def emit_prog_header(self):
        self._write_instr('    jal {}', mangle_label('main'))
        self._write_instr('    li $v0, 10')
        self._write_instr('    syscall')

    def emit_export(self, name, type_of):
        if isinstance(type_of, types.FunctionDecl):
            self._write_instr('.globl {}', mangle_label(name))
        else:
            if name in self.backend.current_context.array_bound:
                size = self.backend.current_context.array_bound[name]
            else:
                size = self.backend._type_size(name)

            self._write_instr('.extern {}, {}', mangle_label(name), size)

    def emit_func_header(self, name):
        self._write_instr('    sw $fp, -4($sp)')
        self._write_instr('    sw $ra, -8($sp)')
        self._write_instr('    addi $fp, $sp, 0')
        self._write_instr('    addi $sp, $sp, -8')

    def emit_func_footer(self):
        self._write_instr('    addi $sp, $sp, 8')
        self._write_instr('    lw $fp, -4($sp)')
        self._write_instr('    lw $ra, -8($sp)')
        self._write_instr('    jr $ra')

    def emit_asm(self, name, code):
        for instr in code.splitlines():
            self._write_instr('{}', instr)

    def emit_move_word(self, move_reg, 
                        src_reg, src_offset,
                        dest_reg, dest_offset):
        self._write_instr('    lw {}, {}({})', move_reg, src_offset, src_reg)
        self._write_instr('    sw {}, {}({})', move_reg, dest_offset, dest_reg)

    def emit_move_byte(self, move_reg,
                        src_reg, src_offset,
                        dest_reg, dest_offset):
        self._write_instr('    lb {}, {}({})', move_reg, src_offset, src_reg)
        self._write_instr('    sb {}, {}({})', move_reg, dest_offset, dest_reg)

    def emit_load_int(self, reg, value):
        self._write_instr('    li {}, {}', reg, value)

    def emit_load_static_addr(self, reg, name):
        self._write_instr('    la {}, {}', reg, mangle_label(name))

    def emit_load_stack_addr(self, reg, offset):
        self._write_instr('    addi {}, $fp, {}', reg, offset)

    def emit_load_stack_word(self, reg, offset):
        self._write_instr('    lw {}, {}($fp)', reg, offset)

    def emit_save_stack_word(self, reg, offset):
        self._write_instr('    sw {}, {}($fp)', reg, offset)

    def emit_load_stack_byte(self, reg, offset):
        self._write_instr('    lb {}, {}($fp)', reg, offset)

    def emit_save_stack_byte(self, reg, offset):
        self._write_instr('    sb {}, {}($fp)', reg, offset)

    def emit_int_to_byte(self, reg):
        self._write_instr('    sll {0}, {0}, 24', reg)
        self._write_instr('    sra {0}, {0}, 24', reg)

    def emit_byte_to_int(self, reg):
        pass

    def emit_array_offset(self, reg, array_offset, index_offset, elem_size):
        self._write_instr('    lw $t0, {}($fp)', array_offset)
        self._write_instr('    lw $t1, {}($fp)', index_offset)
        self._write_instr('    li $t2, {}', elem_size)
        self._write_instr('    mult $t1, $t2')
        self._write_instr('    mflo $t1')
        self._write_instr('    add {}, $t0, $t1', reg)
 
    def emit_left_array_offset(self, reg, array_offset, index_offset, elem_size):
        self._write_instr('    lw $t0, {}($fp)', array_offset)
        self._write_instr('    lw $t1, {}($fp)', index_offset)
        self._write_instr('    li $t2, {}', elem_size)
        self._write_instr('    mult $t1, $t2')
        self._write_instr('    mflo $t1')
        self._write_instr('    sub {}, $t0, $t1', reg)

    def emit_struct_offset(self, reg, total_offset):
        self._write_instr('    addi {0}, {0}, {1}', reg, total_offset)

    def emit_add(self, dest_reg, rhs_reg):
        self._write_instr('    add {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_sub(self, dest_reg, rhs_reg):
        self._write_instr('    sub {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_mul(self, dest_reg, rhs_reg):
        self._write_instr('    mult {}, {}', dest_reg, rhs_reg)
        self._write_instr('    mflo {}', dest_reg)

    def emit_div(self, dest_reg, rhs_reg):
        self._write_instr('    div {}, {}', dest_reg, rhs_reg)
        self._write_instr('    mflo {}', dest_reg)

    def emit_mod(self, dest_reg, rhs_reg):
        self._write_instr('    div {}, {}', dest_reg, rhs_reg)
        self._write_instr('    mfhi {}', dest_reg)

    def emit_less(self, dest_reg, rhs_reg):
        self._write_instr('    slt {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_greater(self, dest_reg, rhs_reg):
        self._write_instr('    sgt {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_lesseq(self, dest_reg, rhs_reg):
        self._write_instr('    sle {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_greateq(self, dest_reg, rhs_reg):
        self._write_instr('    sge {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_eq(self, dest_reg, rhs_reg):
        self._write_instr('    seq {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_noteq(self, dest_reg, rhs_reg):
        self._write_instr('    sne {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_bit_and(self, dest_reg, rhs_reg):
        self._write_instr('    and {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_bit_or(self, dest_reg, rhs_reg):
        self._write_instr('    or {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_shiftleft(self, dest_reg, rhs_reg):
        self._write_instr('    sllv {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_shiftright_arith(self, dest_reg, rhs_reg):
        self._write_instr('    srav {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_shiftright_log(self, dest_reg, rhs_reg):
        self._write_instr('    slav {0}, {0}, {1}', dest_reg, rhs_reg)

    def emit_bit_not(self, reg):
        self._write_instr('    not {}', reg)

    def emit_branch_if_zero(self, reg, label):
        self._write_instr('    beq {}, $0, {}', reg, label)

    def emit_branch_if_nonzero(self, reg, label):
        self._write_instr('    bne {}, $0, {}', reg, label)

    def emit_not(self, reg):
        self._write_instr('    seq {0}, {0}, $0', reg)

    def emit_indirect_call(self, reg):
        self._write_instr('    jalr {}', reg)

    def emit_jump(self, label):
        self._write_instr('    j {}', label)

def get_backend(output, filename, is_library):
    """
    Returns the backend represented by this module.
    """
    return Common32Backend(MarsTemplates(), output, filename, is_library)
