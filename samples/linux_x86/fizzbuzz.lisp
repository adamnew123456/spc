(require "arch/linux_x86.lisp")

(declare
  (str.newline (ascii "\n"))
  (str.fizz (ascii "Fizz"))
  (str.buzz (ascii "Buzz"))

  (strrev (function byte string))
  (strlen (function integer string))
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
  (declare)
  (block
    (linux.write 1 str (strlen str))))

;; Converts the given string to an integer, placing it in the given buffer
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
    (str.intbuff (ascii "   "))
    (i integer)
    (even3 integer)
    (even5 integer))
  (block
    (set i 1)
    (while (<= i 100)
     (block 
      (set even3 (== (% i 3) 0)) 
      (set even5 (== (% i 5) 0)) 

      (if (! (|| even3 even5)) 
       (block
        (itoa i str.intbuff)
        (print str.intbuff))
       (block 
        (if even3 (print str.fizz)) 
        (if even5 (print str.buzz)))) 
      
      (print str.newline) 
      (set i (+ i 1))))))
