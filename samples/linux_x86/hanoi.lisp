(require "arch/linux_x86.lisp")

(declare
 (move-disk (ascii "Move disk "))
 (from-peg (ascii " from Peg "))
 (to-peg (ascii " to Peg "))
 (newline (ascii "\n"))
 (intbuff (ascii "   "))

 (strrev (function byte string))
 (strlen (function integer string))
 (print (function byte string))
 (itoa (function byte integer string))
 (atoi (function integer string))
 (hanoi (function byte
         integer integer integer integer))
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

(define atoi (buffer)
 (declare
  (accum integer)
  (index integer))

 (block
  (set index 0)
  (set accum 0)

  (while (&& (>= (byte-to-int (array buffer index)) 48)
             (<= (byte-to-int (array buffer index)) 57))
   (block
    (set accum (* accum 10))
    (set accum 
     (+ accum (- (byte-to-int (array buffer index)) 48)))

    (set index (+ index 1))))

  (return accum)))

(define hanoi (src dest temp disk)
 (declare)
 (block
  (if (> disk 1)
   (hanoi src temp dest (- disk 1)))

  (print move-disk)

  (itoa disk intbuff)
  (print intbuff)

  (print from-peg)

  (itoa src intbuff)
  (print intbuff)

  (print to-peg)

  (itoa dest intbuff)
  (print intbuff)

  (print newline)

  (if (> disk 1)
   (hanoi temp dest src (- disk 1)))))

(define main ()
 (declare)
 (block
  (linux.read 0 intbuff 3)
  (hanoi 1 3 2 (atoi intbuff))))
