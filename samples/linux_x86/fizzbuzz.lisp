(require "lib/io.lisp")
(require "lib/str.lisp")
(declare
  (fizz (ascii "Fizz"))
  (buzz (ascii "Buzz"))
  (main (function byte)))

(define main ()
  (declare
    (intbuff (ascii "   "))
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
        (str.int->str i intbuff)
        (io.print intbuff))
       (block 
        (if even3 (io.print fizz)) 
        (if even5 (io.print buzz)))) 
      
      (io.printc #\n)
      (set i (+ i 1))))))
