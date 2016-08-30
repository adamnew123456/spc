(namespace if-test)
(require "lib/assert.lisp")
(declare
 (should-not-execute (function byte))
 (main (function byte)))

(define should-not-execute ()
 (declare
  (msg (ascii "This should not have executed")))
 (assert:assert 0 msg))

(define main ()
 (declare)
 (block
  (if 1 1)
  (if 0 (should-not-execute))

  (if 1 1 (should-not-execute))
  (if 0 (should-not-execute) 1)))
