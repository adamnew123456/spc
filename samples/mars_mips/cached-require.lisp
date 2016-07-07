(require "lib/assert.lisp")
(require "lib/io.lisp") ;; Assert requires IO
(declare
 (has-io.print (function integer))
 (main (function byte)))

(define has-io.print ()
 (declare)
 (*if (var-def? io.print)
  (return 1)
  (return 0)))

(define main ()
 (declare
  (msg (ascii "We should have loaded io.print")))
 (assert (== (has-io.print) 1) msg))
