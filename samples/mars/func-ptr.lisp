(require "arch/mars.lisp")

(declare
 (newline (ascii "\n"))
 (double (function integer integer))
 (triple (function integer integer))
 (do-42 (function integer
         (func-pointer integer integer)))
 (main (function byte)))

(define double (x) 
 (declare)
 (return (* x 2)))

(define triple (x)
 (declare)
 (return (* x 3)))

(define do-42 (func)
 (declare)
 (return (func 42)))

(define main ()
 (declare)
 (block
  (mars.print-int (do-42 double))
  (mars.print-string newline)
  (mars.print-int (do-42 triple))
  (mars.print-string newline)))
