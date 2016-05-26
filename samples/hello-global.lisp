(declare
 (hello-world (ascii "Hello, World!\n"))
 (main (function byte))

 (mars.print-string
  (function byte string)))

(import mars.print-string)

(define main ()
 (declare)
 (block
  (mars.print-string hello-world)))
