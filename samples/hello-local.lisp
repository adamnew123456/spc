(declare
 (main (function byte))

 (mars.print-string
  (function byte string)))

(import mars.print-string)

(define main ()
 (declare
  (hello-world (ascii "Hello, World!\n")))
 (block
  (mars.print-string hello-world)))
