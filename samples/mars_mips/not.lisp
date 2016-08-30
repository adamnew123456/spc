(namespace not-test)
(require "lib/assert.lisp")
(declare
 (main (function byte)))

(define main ()
 (declare
  (msg.1 (ascii "(! 0) should be 1"))
  (msg.2 (ascii "(! 1) should be 0")))
 (block
  (assert:assert (! 0) msg.1)
  (assert:assert (! (! 1)) msg.2)))
