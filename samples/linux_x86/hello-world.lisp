(require "arch/linux_x86.lisp")

(declare
 (str.hello (ascii "Hello, World!\n"))

 (strlen (function integer string))
 (print (function byte string))
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

(define main ()
 (declare)
 (block
  (print str.hello)))
