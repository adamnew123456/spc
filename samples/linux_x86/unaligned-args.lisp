(declare
 
 ;; Ensure that the compiler takes into account alignment of arguments when pushing 
 ;; values
 (unaligned (function integer byte integer))
 (main (function byte)))

(define unaligned (a b)
 (declare)
 (return b))

(define main ()
 (declare)
 (unaligned (int-to-byte 42) 7))
