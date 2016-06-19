(require "arch/linux_x86.lisp")

(declare
  (str.intbuff (ascii "   "))
  (str.fizz (ascii "Fizz"))
  (str.buzz (ascii "Buzz"))
  (str.newline (ascii "\n"))
  (str.space (ascii " "))

  (pow10 (function integer))
  (strlen (function integer string))
  (print (function byte string))
  (atoi (function string integer string integer))
  (main (function byte)))

;; Computes the length of the given string
(define strlen (str)
  (declare
   (i integer))
  (block
   (set i 0)
   (while (!= 0 (array str i)) 
    (set i (+ i 1)))
   
   (return (- i 1))))

;; Writes the given string to standard output
(define print (str)
  (declare)
  (block
    (linux.write 1 str (strlen str))))

;; Computes 10^x, where x >= 0
(define pow10 (x)
  (declare
    (result integer))
  (block
    (set result 1)
    (while (> x 0)
           (set result (* result 10))
           (set x (- x 1)))
    (return result)))

;; Converts the given string to an integer, placing it in the given buffer
(define atoi (x buffer buffsz)
 (declare
  (buffptr string))
 (block
  (set buffptr (int-to-ptr (+ (ptr-to-int buffer) (- buffsz 1)) string))

  (if (== x 0)
    (block
      (set (deref buffptr) (byte-to-int 48))
      (return buffptr))

    (block
     (while (&& (<= buffer buffptr) (> x 0))
      (block
       (set (deref buffptr) (byte-to-int (+ 48 (% x 10))))
       (set buffptr (ptr-to-int (- (int-to-ptr buffptr) 1) string))

       (set x (/ x 10))))

     ;; The buffptr adjustment in the loop over-adjusts, since it always runs, 
     ;; even when x hits zero and there are no more digits
     (set buffptr (ptr-to-int (+ (int-to-ptr buffptr) 1) string))
     (return buffptr)))))

(define main ()
  (declare
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
       (print (atoi i str.intbuff 3))
       (block 
        (if even3 (print str.fizz)) 
        (if even5 (print str.buzz)))) 
      
      (print str.newline) 
      (set i (+ i 1))))))
