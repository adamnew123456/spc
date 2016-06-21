(require "arch/mars.lisp")
    
(declare
 (newline (array-of byte 2))
 (int-pair
  (struct (a integer)
          (b integer)))
 
 (main
  (function byte)))

(define main ()
 (declare (pair int-pair))
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (set (field pair a) 1)
  (set (field pair b) 2)

  (mars.print-int (field pair a))
  (mars.print-string newline)
  (mars.print-int (field pair b))
  (mars.print-string newline)))
