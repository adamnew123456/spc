(require "lib/assert.lisp")
(declare
 (three-param (function byte integer integer integer))
 (main (function byte)))

(define three-param (a b c)
 (declare
  (msg.1 (ascii "a should be 1"))
  (msg.2 (ascii "b should be 2"))
  (msg.3 (ascii "c should be 3")))
 (block
  (assert (== a 1) msg.1)
  (assert (== b 2) msg.2)
  (assert (== c 3) msg.3)))

(define main ()
 (declare)
 (block
  (three-param 1 2 3)))
