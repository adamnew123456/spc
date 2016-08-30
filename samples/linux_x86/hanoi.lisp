(namespace hanoi-test)
(require "lib/str.lisp")
(require "lib/io.lisp")

(declare
 (move-disk (ascii "Move disk "))
 (from-peg (ascii " from Peg "))
 (to-peg (ascii " to Peg "))
 (intbuff (ascii "     "))

 (hanoi (function byte
         integer integer integer integer))
 (main (function byte)))

(define hanoi (src dest temp disk)
 (declare)
 (block
  (if (> disk 1)
   (hanoi src temp dest (- disk 1)))

  (io:print move-disk)

  (str:int->str disk intbuff)
  (io:print intbuff)

  (io:print from-peg)

  (str:int->str src intbuff)
  (io:print intbuff)

  (io:print to-peg)

  (str:int->str dest intbuff)
  (io:print intbuff)

  (io:printc #\n)

  (if (> disk 1)
   (hanoi temp dest src (- disk 1)))))

(define main ()
 (declare)
 (block
  (hanoi 1 3 2 (io:read-int))))
