(require "arch/linux_x86.lisp")

(declare
  (strlen (function integer string))
  (print (function byte string))
  (println (function byte string))
  (assert (function byte integer string))
  (main (function byte)))

;; Computes the length of the given string
(define strlen (str)
  (declare
   (i integer))
  (block
   (set i 0)
   (while (!= 0 (byte-to-int (array str i)))
    (set i (+ i 1)))

   (return i)))

;; Writes the given string to standard output
(define print (str)
  (declare)
  (block
    (linux.write 1 str (strlen str))))

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
