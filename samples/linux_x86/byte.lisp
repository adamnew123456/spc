(namespace byte-sample)
(require "lib/assert.lisp")
(declare
 (main (function byte)))

(define main ()
 (declare
  (x byte)
  (y (ascii "q\n"))
  
  (msg.1 (ascii "Byte literal 'q' should be equal to y[0]"))
  (msg.2 (ascii "Byte literal '\n' should be equal to y[1]")))

 (block
  (set x #q)
  (assert:assert (== x (array y 0)) msg.1)

  (set x #\n)
  (assert:assert (== x (array y 1)) msg.2)))
