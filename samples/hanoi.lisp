(declare
 (move-disk (ascii "Move disk "))
 (from-peg (ascii " from Peg "))
 (to-peg (ascii " to Peg "))
 (newline (ascii "\n"))

 (hanoi (function byte
         integer integer integer integer))
 (main (function byte))

 (mars.print-string
  (function byte string))
 
 (mars.print-int
  (function byte integer))

 (mars.read-int
  (function integer)))

(import mars.print-string mars.print-int mars.read-int)

(define hanoi (src dest temp disk)
 (declare)
 (block
  (if (> disk 1)
   (hanoi src temp dest (- disk 1)))

  (mars.print-string move-disk)
  (mars.print-int disk)
  (mars.print-string from-peg)
  (mars.print-int src)
  (mars.print-string to-peg)
  (mars.print-int dest)
  (mars.print-string newline)

  (if (> disk 1)
   (hanoi temp dest src (- disk 1)))))

(define main ()
 (declare)
 (block
  (hanoi 1 3 2 (mars.read-int))))
