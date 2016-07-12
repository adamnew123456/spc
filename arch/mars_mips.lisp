(declare
 (mars.print-int (function byte integer))
 (mars.print-string (function byte string))
 (mars.read-int (function integer))
 (mars.read-string (function byte string integer))
 (mars.sbrk (function (pointer-to byte) integer))
 (mars.exit (function byte)))

(export 'mars.print-int 'mars.print-string
        'mars.read-int 'mars.read-string
        'mars.sbrk 'mars.exit)

(assemble mars.print-int
 "lw $a0, 0($sp)
  li $v0, 1
  syscall
  jr $ra")

(assemble mars.print-string
 "lw $a0, 0($sp)
  li $v0, 4
  syscall
  jr $ra")

(assemble mars.read-int
 "li $v0, 5
  syscall
  jr $ra")

(assemble mars.read-string
 "lw $a0, 0($sp)
  lw $a1  4($sp)
  li $v0, 8
  syscall
  jr $ra")

(assemble mars.sbrk
 "lw $a0, 0($sp)
  li $v0, 9
  syscall
  jr $ra")

(assemble mars.exit
 "li $v0, 10
  syscall")
