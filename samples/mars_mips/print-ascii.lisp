(require "arch/mars.lisp")
    
(declare
 (text (array-of byte 3))
 (main (function byte)))

(define main ()
 (declare)
 (block
  (set (array text 0) 33) ;; !
  (set (array text 1) 10) ;; \n
  (set (array text 2) 0) ;; \0

  (mars.print-string text)))
