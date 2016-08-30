(namespace require-hiding-test)
(require "lib/assert.lisp") ;; Ensure that we don't get io.print, 
                            ;; even though assert does

(declare
 (has-io-print (function integer))
 (main (function byte)))

(define has-io-print ()
 (declare)
 (*if (var-def? io:print)
  (return 1)
  (return 0)))

(define main ()
 (declare
  (msg (ascii "We should not get io.print")))
 (assert:assert (== (has-io-print) 0) msg))
