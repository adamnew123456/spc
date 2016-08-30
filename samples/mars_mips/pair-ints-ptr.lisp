(namespace pair-ints-ptr-test)
(require "lib/assert.lisp")
(declare
 (int-pair
  (struct (a integer)
          (b integer)))

 (int-pair-ptr
  (alias (pointer-to int-pair)))
 
 (main
  (function byte)))

(define main ()
 (declare 
  (pair int-pair-ptr)
  (msg.1 (ascii "pair->a should be 1"))
  (msg.2 (ascii "pair->b should be 2")))
 (block
  (set pair (cast int-pair-ptr (mars.sbrk (size-of int-pair))))
  (set (field (deref pair) a) 1)
  (set (field (deref pair) b) 2)

  (assert:assert (== (field (deref pair) a) 1) msg.1)
  (assert:assert (== (field (deref pair) b) 2) msg.1)))
