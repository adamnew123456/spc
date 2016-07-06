(require "lib/assert.lisp")
(declare
 (should-not-execute (function integer))
 (main (function byte)))

(define should-not-execute ()
 (declare
  (msg (ascii "This should ! have executed")))
 (assert 0 msg))

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
  (assert (|| 1 (should-not-execute)) msg.1)
  (assert (|| 0 1) msg.2)
  (assert (! (|| 0 0)) msg.3)

  (assert (&& 1 1) msg.4)
  (assert (! (&& 0 (should-not-execute))) msg.5)
  (assert (! (&& 1 0)) msg.6)))
