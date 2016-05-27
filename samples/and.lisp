(declare
 (main (function byte))
 (mars.print-int (function byte integer)))

(import mars.print-int)

(define main ()
 (declare)
 (block
  (mars.print-int (&& 0 1))))
