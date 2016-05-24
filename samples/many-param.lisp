(declare
 (newline (array-of byte 3))
 (three-param (function byte integer integer integer))
 (main (function byte)))

(define three-param (a b c)
 (declare)
 (block
  (@print-int a)
  (@print-string newline)

  (@print-int b)
  (@print-string newline)

  (@print-int c)
  (@print-string newline)))

(define main ()
 (declare)
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (three-param 1 2 3)))
