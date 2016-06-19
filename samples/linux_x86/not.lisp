(require "arch/linux_x86.lisp")

(declare
 (breakpoint (function byte))
 (say-yes (function integer integer))
 (say-no (function integer integer))
 (main (function byte)))

(define breakpoint ()
 (declare)
 (block))

(define say-yes (x)
 (declare
  (str.yes (ascii "YES\n")))
 (block
  (linux.write 1 str.yes 4)
  (return x)))

(define say-no (x)
 (declare
  (str.no (ascii "NO\n")))
 (block
  (linux.write 1 str.no 3)
  (return x)))

(define main ()
 (declare)

 (block
  (if 1 (say-yes 0))
  (if 0 (say-no 0))

  (if 1 (say-yes 0) (say-no 0))
  (if 0 (say-no 0) (say-yes 0))

  (if (! 1) (say-no 0))
  (if (! 0) (say-yes 0))

  (if (! 1) (say-no 0) (say-yes 0))
  (if (! 0) (say-yes 0) (say-no 0))))
