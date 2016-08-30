(namespace arith-sample)
(require "lib/assert.lisp")
(declare
 (main (function byte)))

(define main ()
 (declare
  (x integer)
  (y integer)
  (z integer)

  (msg.1 (ascii "(+ x y) should be 11"))
  (msg.2 (ascii "(- x y) should be 5"))
  (msg.3 (ascii "(* x y) should be 24"))
  (msg.4 (ascii "(/ x y) should be 2"))
  (msg.5 (ascii "(% x 7) should be 1")))

 (block
  (set x 8)
  (set y 3)

  (assert:assert (== (+ x y) 11) msg.1)
  (assert:assert (== (- x y) 5) msg.2)
  (assert:assert (== (* x y) 24) msg.3)
  (assert:assert (== (/ x y) 2) msg.4)
  (assert:assert (== (% x 7) 1) msg.5)))
