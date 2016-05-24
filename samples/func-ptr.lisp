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
  (@print-int (do-42 double))
  (@print-string newline)
  (@print-int (do-42 triple))
  (@print-string newline)))
