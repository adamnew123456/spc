(namespace static-if-test)
(require "lib/assert.lisp")

(declare
  (is-linux (function integer))
  (is-mars (function integer))
  (main (function byte)))

(define is-linux ()
 (declare)
 (block
  (*if (platform? "linux" "*")
   (return 1)
   (return 0))))

(define is-mars ()
 (declare)
 (block
  (*if (platform? "mars" "mips")
   (return 1)
   (return 0))))

(define main ()
 (declare
  (msg.1 (ascii "is-linux should be 1"))
  (msg.2 (ascii "is-mars should be 0")))
 (block
  (assert:assert (== 0 (is-linux)) msg.1)
  (assert:assert (== 1 (is-mars)) msg.2)))
