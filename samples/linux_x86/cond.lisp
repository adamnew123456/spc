(require "arch/linux_x86.lisp")

(declare
 (str.yes (ascii "YES\n"))
 (str.no (ascii "NO\n"))

 (breakpoint (function byte))
 (say-yes (function integer integer))
 (say-no (function integer integer))
 (main (function byte)))

(define breakpoint ()
 (declare)
 (block))

(define say-yes (x)
 (declare)
 (block
  (linux.write 1 str.yes 4)
  (return x)))

(define say-no (x)
 (declare)
 (block
  (linux.write 1 str.no 3)
  (return x)))

(define main ()
 (declare
  (x integer))
 (block
  (set x (|| (say-yes 1) (say-no 0)))
  (breakpoint) ;; 1 OK

  (set x (|| (say-yes 0) (say-yes 1)))
  (breakpoint) ;; 1 OK

  (set x (|| (say-yes 0) (say-yes 0)))
  (breakpoint) ;; 0 OK

  (set x (&& (say-yes 1) (say-yes 1)))
  (breakpoint) ;; 1 OK

  (set x (&& (say-yes 0) (say-no 1)))
  (breakpoint) ;; 0 OK

  (set x (&& (say-yes 0) (say-no 0)))
  (breakpoint) ;; 0 OK
 ))
