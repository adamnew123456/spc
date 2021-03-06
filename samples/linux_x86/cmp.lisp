(namespace cmp-sample)
(require "lib/assert.lisp")
(declare
 (main (function byte)))

(define main ()
 (declare
  (i integer)
  (j integer)
  (k integer)

  (msg.1 (ascii "j > i"))
  (msg.2 (ascii "i < j"))
  (msg.3 (ascii "j >= i"))
  (msg.4 (ascii "j >= j"))
  (msg.5 (ascii "i <= j"))
  (msg.6 (ascii "i <= i"))
  (msg.7 (ascii "! (i == j)"))
  (msg.8 (ascii "i == i")))

 (block
  (set i 10)
  (set j 100)

  (assert:assert (> j i) msg.1)
  (assert:assert (< i j) msg.2)

  (assert:assert (>= j i) msg.3)
  (assert:assert (>= j j) msg.4)

  (assert:assert (<= i j) msg.5)
  (assert:assert (<= i i) msg.6)

  (assert:assert (! (== i j)) msg.7)
  (assert:assert (!= i j) msg.8)))
