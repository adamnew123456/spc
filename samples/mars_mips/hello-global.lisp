(namespace hello-global-test)
(require "lib/io.lisp")
(declare
 (hello-world (ascii "Hello, World!\n"))
 (main (function byte)))

(define main ()
 (declare)
 (block
  (io:print hello-world)))
