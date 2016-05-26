.text
.globl L_mars_46_print_45_int
.globl L_mars_46_print_45_string
.globl L_mars_46_read_45_int
.globl L_mars_46_read_45_string
.globl L_mars_46_sbrk
.globl L_mars_46_exit

L_mars_46_print_45_int:
    lw $a0, 0($sp)
    li $v0, 1
    syscall
    jr $ra

L_mars_46_print_45_string:
    lw $a0, 0($sp)
    li $v0, 4
    syscall
    jr $ra

L_mars_46_read_45_int:
    li $v0, 5
    syscall
    jr $ra

L_mars_46_read_45_string:
    lw $a0, 0($sp)
    lw $a1  4($sp)
    li $v0, 8
    syscall
    jr $ra

L_mars_46_sbrk:
    lw $a0, 0($sp)
    li $v0, 9
    syscall
    jr $ra

L_mars_46_exit:
    li $v0, 10
    syscall
