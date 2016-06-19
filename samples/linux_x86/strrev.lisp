(require "arch/linux_x86.lisp")

(declare
  (str.buffer (ascii "HELLO"))
  (str.newline (ascii "\n"))

  (strlen (function integer string))
  (strrev (function byte string))
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
   
;; Reverses the given string
(define strrev (str)
 (declare
  (i integer)
  (j integer)
  (tmp byte))

 (block
  (set i 0)
  (set j (- (strlen str) 1))

  (while (> j i)
   (block
    (set tmp (array str i))
    (set (array str i) (array str j))
    (set (array str j) tmp)

    (set i (+ i 1))
    (set j (- j 1))))))

;; Writes the given string to standard output
(define print (str)
  (declare)
  (block
    (linux.write 1 str (strlen str))))

(define main ()
 (declare)
 (block
  (strrev str.buffer)
  (print str.buffer)
  (print str.newline)))
