(require "lib/io.lisp")
(declare
  (assert (function byte integer string)))

(export 'assert)

(define assert (cond msg)
 (declare)
 (block
  (if (! cond) (io.println msg))))
