(declare
 (newline (array-of byte 2))

 (linked-ints
  (struct (value integer)
          (next (pointer-to linked-ints))))

 (cons
  (function (pointer-to linked-ints)
   integer (pointer-to linked-ints)))

 (main (function byte))

 (mars.sbrk
  (function (pointer-to byte) integer))

 (mars.print-int
  (function byte integer))

 (mars.print-string
  (function byte string)))

(import mars.print-int mars.print-string mars.sbrk)

(define cons (num rest)
 (declare
  (list (pointer-to linked-ints)))

 (block
  (set list (cast (pointer-to linked-ints) (mars.sbrk (size-of linked-ints))))
  (set (field (deref list) value) num)
  (set (field (deref list) next) rest)
  (return list)))

(define main ()
 (declare
  (list (pointer-to linked-ints)))
 
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (set list (int-to-ptr (pointer-to linked-ints) 0))
  (set list (cons 1 list))
  (set list (cons 2 list))

  (mars.print-int (field (deref list) value))
  (mars.print-string newline)

  (set list (field (deref list) next))
  (mars.print-int (field (deref list) value))
  (mars.print-string newline)

  (mars.print-int (ptr-to-int (field (deref list) next)))
  (mars.print-string newline)))
