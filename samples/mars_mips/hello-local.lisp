(require "lib/io.lisp")
(declare
 (main (function byte)))

(define main ()
 (declare
  (hello-world (ascii "Hello, World!\n")))
 (block
  (io.print hello-world)))
