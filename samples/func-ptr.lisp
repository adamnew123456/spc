(declare
 (newline (ascii "\n"))
 (double (function integer integer))
 (triple (function integer integer))
 (do-42 (function integer
         (func-pointer integer integer)))
 (main (function byte))

 (mars.print-int
  (function byte integer))
 
 (mars.print-string
  (function byte string)))

(import mars.print-int mars.print-string)

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
