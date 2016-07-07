(require "lib/assert.lisp")
(declare
  (main (function byte)))

(define main ()
 (declare
  (msg.1 (ascii "Should hit case #1"))
  (msg.2 (ascii "Should hit case #2"))
  (msg.3 (ascii "Should hit else case"))
  (msg.4 (ascii "Should hit no case")))
 (block
  (switch
   (case (== 1 1) 1)
   (case (== 1 0) (assert 0 msg.1)))

  (switch
   (case (== 1 0) (assert 0 msg.2))
   (case (== 1 1) 1))

  (switch
   (case (== 1 0) (assert 0 msg.3))
   (else 1))

  (switch
   (case (== 1 0) (assert 0 msg.4)))))
