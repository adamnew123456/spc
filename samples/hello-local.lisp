(require "arch/mars.lisp")
    
(declare
 (main (function byte)))

(define main ()
 (declare
  (hello-world (ascii "Hello, World!\n")))
 (block
  (mars.print-string hello-world)))
