(require "arch/linux_x86.lisp")

(declare
  (strlen (function integer string))
  (print (function byte string))
  (println (function byte string))
  (is-linux (function integer))
  (is-mars (function integer))
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

(define is-linux ()
 (declare)
 (block
  (*if (platform? "linux_x86")
   (return 1)
   (return 0))))

(define is-mars ()
 (declare)
 (block
  (*if (platform? "mars")
   (return 1)
   (return 0))))

(define main ()
 (declare
  (msg.1 (ascii "is-linux should be 0"))
  (msg.2 (ascii "is-mars should be 1")))
 (block
  (assert (== 0 (is-linux)) mars.1)
  (assert (== 1 (is-mars)) mars.2)))
