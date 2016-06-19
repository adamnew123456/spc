(require "arch/mars.lisp")
    
(declare
 (main (function byte))
 (mars.print-int (function byte integer)))

(define main ()
 (declare)
 (block
  (mars.print-int (|| 0 1))))
