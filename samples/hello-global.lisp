(declare
 (hello-world (ascii "Hello, World!\n"))
 (main (function byte)))

(define main ()
 (declare)
 (block
  (@print-string hello-world)))
