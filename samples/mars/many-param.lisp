(require "arch/mars.lisp")
    
(declare
 (newline (array-of byte 3))
 (three-param (function byte integer integer integer))
 (main (function byte)))

(define three-param (a b c)
 (declare)
 (block
  (mars.print-int a)
  (mars.print-string newline)

  (mars.print-int b)
  (mars.print-string newline)

  (mars.print-int c)
  (mars.print-string newline)))

(define main ()
 (declare)
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (three-param 1 2 3)))
