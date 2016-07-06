(require "lib/io.lisp")
(declare
 (str.hello (ascii "Hello, World!\n"))
 (main (function byte)))

(define main ()
 (declare)
 (block
  (io.print str.hello)))
