(require "lib/io.lisp")
(require "arch/linux_x86.lisp")

(declare
 (dquote (ascii "'"))
 (newline (ascii "\n"))
 (buff (array-of byte 101))

 (main (function byte)))

(define main ()
 (declare)
 (block
  (set (array buff 100) 0)
  (linux.read 0 buff 100)

  (io.printc #')
  (io.print buff)
  (io.printc #')
  (io.printc #\n)))
