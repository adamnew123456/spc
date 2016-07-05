(require "arch/mars.lisp")

(declare
  (print (function byte string))
  (println (function byte string))
  (assert (function byte integer string))
  (main (function byte)))

;; Writes the given string to standard output
(define print (str)
  (declare)
  (block
    (mars.print-string str)))

(define println (str)
  (declare
    (newline (ascii "\n")))
  (block
    (print str)
    (print newline)))

;; Asserts that the given condition is true, or not
(define assert (cond message)
 (declare)
 (block
  (if (! cond) (println message))))

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
