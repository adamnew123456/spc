(require "lib/assert.lisp")
(require "arch/mars.lisp")
(declare
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
  (set list (cast (pointer-to linked-ints) (mars.sbrk (size-of linked-ints))))
  (set (field (deref list) value) num)
  (set (field (deref list) next) rest)
  (return list)))

(define main ()
 (declare
  (list (pointer-to linked-ints))
  (msg.1 (ascii "List[0] should be 2"))
  (msg.2 (ascii "List[1] should be 1"))
  (msg.3 (ascii "List[2] should be 0"))
  (msg.4 (ascii "List[3] should be NULL")))
 
 (block
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (set list (int-to-ptr (pointer-to linked-ints) 0))
  (set list (cons 1 list))
  (set list (cons 2 list))

  (assert (== (field (deref list) value) 2) msg.1)

  (set list (field (deref list) next))
  (assert (== (field (deref list) value) 1) msg.2)

  (set list (field (deref list) next))
  (assert (== (field (deref list) value) 0) msg.3)

  (set list (field (deref list) next))
  (assert (== list 0) msg.4)))
