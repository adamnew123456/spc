(declare
 (newline (array-of byte 2))

 (linked-ints
  (struct (value integer)
          (next (pointer-to linked-ints))))

 (cons
  (function (pointer-to linked-ints)
   integer (pointer-to linked-ints)))

 (main (function byte)))

(define cons (num rest)
 (declare
  (list (pointer-to linked-ints)))

 (block
  (set list (cast (pointer-to linked-ints) (@sbrk (size-of linked-ints))))
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

  (@print-int (field (deref list) value))
  (@print-string newline)

  (set list (field (deref list) next))
  (@print-int (field (deref list) value))
  (@print-string newline)

  (@print-int (ptr-to-int (field (deref list) next)))
  (@print-string newline)))
