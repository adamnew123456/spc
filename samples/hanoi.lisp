(declare
 (move-disk (ascii "Move disk "))
 (from-peg (ascii " from Peg "))
 (to-peg (ascii " to Peg "))
 (newline (ascii "\n"))

 (hanoi (function byte
         integer integer integer integer))
 (main (function byte)))

(define hanoi (src dest temp disk)
 (declare)
 (block
  (if (> disk 1)
   (hanoi src temp dest (- disk 1)))

  (@print-string move-disk)
  (@print-int disk)
  (@print-string from-peg)
  (@print-int src)
  (@print-string to-peg)
  (@print-int dest)
  (@print-string newline)

  (if (> disk 1)
   (hanoi temp dest src (- disk 1)))))

(define main ()
 (declare)
 (block
  (hanoi 1 3 2 (@read-int))))
