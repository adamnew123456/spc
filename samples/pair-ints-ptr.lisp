(declare
 (newline (array-of byte 2))
 (int-pair
  (struct (a integer)
          (b integer)))

 (int-pair-ptr
  (alias (pointer-to int-pair)))
 
 (main
  (function byte)))

(define main ()
 (declare (pair int-pair-ptr))
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (set pair (cast int-pair-ptr (@sbrk (size-of int-pair))))
  (set (field (deref pair) a) 1)
  (set (field (deref pair) b) 2)

  (@print-int (field (deref pair) a))
  (@print-string newline)
  (@print-int (field (deref pair) b))
  (@print-string newline)))
