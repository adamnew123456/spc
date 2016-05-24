(declare
 (main (function byte)))

(define main ()
 (declare
  (hello-world (ascii "Hello, World!\n")))
 (block
  (@print-string hello-world)))
