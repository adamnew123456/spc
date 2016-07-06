(require "lib/assert.lisp")
(declare
 (int-pair
  (struct (a integer)
          (b integer)))
 
 (main
  (function byte)))

(define main ()
 (declare 
  (pair int-pair)
  (msg.1 (ascii "pair.a should be 1"))
  (msg.2 (ascii "pair.b should be 2")))
 (block
  (set (field pair a) 1)
  (set (field pair b) 2)

  (assert (== (field pair a) 1) msg.1)
  (assert (== (field pair b) 2) msg.2)))
