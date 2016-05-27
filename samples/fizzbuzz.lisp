(declare
 (mars.print-int (function byte integer))
 (mars.print-string (function byte string))

 (str.fizz (ascii "Fizz"))
 (str.buzz (ascii "Buzz"))
 (str.newline (ascii "\n"))
 (str.space (ascii " "))

 (main (function byte)))

(import mars.print-int mars.print-string)

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
     (mars.print-int i)
     (block
      (if even3 (mars.print-string str.fizz))
      (if even5 (mars.print-string str.buzz))))

    (mars.print-string str.newline)
    (set i (+ i 1))))))
