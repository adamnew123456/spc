(declare
 (text (array-of byte 3))
 (main (function byte)))

(define main ()
 (declare)
 (block
  (set (array text 0) (int-to-byte 33)) ;; !
  (set (array text 1) (int-to-byte 10)) ;; \n
  (set (array text 2) (int-to-byte 0)) ;; \0
  (@print-string text)))
