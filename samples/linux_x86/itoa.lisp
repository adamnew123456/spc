(require "arch/linux_x86.lisp")

(declare
  (strlen (function integer string))
  (strrev (function byte string))
  (print (function byte string))
  (itoa (function byte integer string))
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
  (declare
   (newline (ascii "\n")))
  (block
    (linux.write 1 str (strlen str))
    (linux.write 1 newline 1)))

(define itoa (x buffer)
 (declare
  (index integer))

 (block
  (if (== x 0)
   ;; Trivial case - for 0, just put a 0 and close the buffer
   (block
    (set (array buffer 0) (int-to-byte 48))
    (set (array buffer 1) (int-to-byte 0)))
   
   (block
    (set index 0)
    (while (> x 0)
     (block
      (set (array buffer index) (int-to-byte (+ (% x 10) 48)))
      (set index (+ index 1))
      (set x (/ x 10))))

    (set (array buffer index) (int-to-byte 0))
    (strrev buffer)))))

(define main ()
 (declare
  (str.intbuff (ascii "   ")))
 (block
  (itoa 304 str.intbuff)
  (print str.intbuff)

  (itoa 0 str.intbuff)
  (print str.intbuff)

  (itoa (! 0) str.intbuff)
  (print str.intbuff)

  (itoa (! 1) str.intbuff)
  (print str.intbuff)))
