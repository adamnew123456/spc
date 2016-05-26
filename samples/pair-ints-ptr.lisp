(declare
 (newline (array-of byte 2))
 (int-pair
  (struct (a integer)
          (b integer)))

 (int-pair-ptr
  (alias (pointer-to int-pair)))
 
 (main
  (function byte))

 (mars.print-int
  (function byte integer))

 (mars.print-string
  (function byte string))

 (mars.sbrk
  (function (pointer-to byte) integer)))

(import mars.print-int mars.print-string mars.sbrk)

(define main ()
 (declare (pair int-pair-ptr))
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (set pair (cast int-pair-ptr (mars.sbrk (size-of int-pair))))
  (set (field (deref pair) a) 1)
  (set (field (deref pair) b) 2)

  (mars.print-int (field (deref pair) a))
  (mars.print-string newline)
  (mars.print-int (field (deref pair) b))
  (mars.print-string newline)))
