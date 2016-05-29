(require "arch/mars.lisp")

(declare
 (hello-world (ascii "Hello, World!\n"))
 (main (function byte)))

(define main ()
 (declare)
 (block
  (mars.print-string hello-world)))
