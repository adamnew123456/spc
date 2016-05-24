(declare
 (jagged (struct (a integer)
                 (b byte)))

 (takes-jagged (function byte jagged))
 (requires-right-alignment (function integer integer))
 (main (function byte)))

(define takes-jagged (jagged)
 (declare)
 (block))

(define requires-right-alignment (a)
 (declare)
 (block
  (return a)))

(define main ()
 (declare (jag jagged))
 (block
  (takes-jagged jag)
  (requires-right-alignment 5)))
