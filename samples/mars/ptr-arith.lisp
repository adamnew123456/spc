(require "lib/assert.lisp")
(declare
  (main (function byte)))

(define main ()
 (declare
  (x (array-of integer 3))
  (ptr (pointer-to integer))

  (msg.1 (ascii "(!= (deref ptr) 1)"))
  (msg.2 (ascii "(!= (deref ptr) 2)"))
  (msg.3 (ascii "(!= (deref ptr) 3)"))
  (msg.4 (ascii "(!= (deref ptr) 1)")))
 (block
  (set (array x 0) 1)
  (set (array x 1) 2)
  (set (array x 2) 3)

  (set ptr x)
  (assert (== (deref ptr) 1) msg.1)

  (set ptr (+ ptr 1))
  (assert (== (deref ptr) 2) msg.2)

  (set ptr (+ ptr 1))
  (assert (== (deref ptr) 3) msg.3)

  (set ptr (- ptr 2))
  (assert (== (deref ptr) 1) msg.4)))
