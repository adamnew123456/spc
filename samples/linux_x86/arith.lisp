(declare
 (main (function byte))
 (breakpoint (function byte)))

(define breakpoint ()
 (declare)
 (block))

(define main ()
 (declare
  (x integer)
  (y integer)
  (z integer))

 (block
  (set x 8)
  (set y 3)

  (breakpoint)
  (set z (+ x y)) ;; 11
  (breakpoint)
  (set z (- x y)) ;; 5
  (breakpoint)
  (set z (* x y)) ;; 24
  (breakpoint)
  (set z (/ x y)) ;; 2
  (breakpoint)
  (set z (% x 7)) ;; 1
