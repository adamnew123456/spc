(require "lib/io.lisp")

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

  (io.print dquote)
  (io.print buff)
  (io.print dquote)
  (io.print newline)))
