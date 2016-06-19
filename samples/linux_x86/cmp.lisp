(require "arch/linux_x86.lisp")

(declare
 (str.yes (ascii "YES\n"))
 (str.no (ascii "NO\n"))

 (say-yes (function byte))
 (say-no (function byte))
 (main (function byte)))

(define say-yes ()
 (declare)
 (block
  (linux.write 1 str.yes 4)))

(define say-no ()
 (declare)
 (block
  (linux.write 1 str.no 3)))

(define main ()
 (declare
  (i integer)
  (j integer))

 (block
  (set i 10)
  (set j 100)

  (if (> j i) (say-yes) (say-no))

  (if (< i j) (say-yes) (say-no))

  (if (>= j i) (say-yes) (say-no))
  (if (>= j j) (say-yes) (say-no))

  (if (<= i j) (say-yes) (say-no))
  (if (<= i i) (say-yes) (say-no))

  (if (== i j) (say-no) (say-yes))
  (if (!= i j) (say-yes) (say-no))))
