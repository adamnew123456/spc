(namespace mars)
(declare
 (print-int (function byte integer))
 (print-string (function byte string))
 (read-int (function integer))
 (read-string (function byte string integer))
 (sbrk (function (pointer-to byte) integer))
 (exit (function byte)))

(export 'print-int 'print-string
        'read-int 'read-string
        'sbrk 'exit)

(assemble print-int
 "lw $a0, 0($sp)
  li $v0, 1
  syscall
  jr $ra")

(assemble print-string
 "lw $a0, 0($sp)
  li $v0, 4
  syscall
  jr $ra")

(assemble read-int
 "li $v0, 5
  syscall
  jr $ra")

(assemble read-string
 "lw $a0, 0($sp)
  lw $a1  4($sp)
  li $v0, 8
  syscall
  jr $ra")

(assemble sbrk
 "lw $a0, 0($sp)
  li $v0, 9
  syscall
  jr $ra")

(assemble exit
 "li $v0, 10
  syscall")
