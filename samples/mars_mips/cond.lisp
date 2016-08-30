(namespace cond-test)
(require "lib/assert.lisp")
(declare
 (should-not-execute (function integer))
 (main (function byte)))

(define should-not-execute ()
 (declare
  (msg (ascii "This should ! have executed")))
 (assert:assert 0 msg))

(define main ()
 (declare
  (x integer)

  (msg.1 (ascii "(|| 1 0)"))
  (msg.2 (ascii "(|| 0 1)"))
  (msg.3 (ascii "(! (|| 0 0))"))
  (msg.4 (ascii "(&& 1 1)"))
  (msg.5 (ascii "(! (&& 0 1))"))
  (msg.6 (ascii "(! (&& 1 0))")))
 (block
  (assert:assert (|| 1 (should-not-execute)) msg.1)
  (assert:assert (|| 0 1) msg.2)
  (assert:assert (! (|| 0 0)) msg.3)

  (assert:assert (&& 1 1) msg.4)
  (assert:assert (! (&& 0 (should-not-execute))) msg.5)
  (assert:assert (! (&& 1 0)) msg.6)))
