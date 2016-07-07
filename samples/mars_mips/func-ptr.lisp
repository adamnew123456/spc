(require "lib/assert.lisp")
(declare
 (newline (ascii "\n"))
 (double (function integer integer))
 (triple (function integer integer))
 (do-42 (function integer
         (func-pointer integer integer)))
 (main (function byte)))

(define double (x) 
 (declare)
 (return (* x 2)))

(define triple (x)
 (declare)
 (return (* x 3)))

(define do-42 (func)
 (declare)
 (return (func 42)))

(define main ()
 (declare
  (msg.1 (ascii "(do-42 double) should be 84"))
  (msg.2 (ascii "(do-42 triple) should be 126")))
 (block
  (assert (== (do-42 double) 84) msg.1)
  (assert (== (do-42 triple) 126) msg.2)))
