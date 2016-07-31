(declare 
 (str.rev (function byte string))
 (str.len (function integer string))
 (str.int->str (function byte integer string))
 (str.str->int (function integer string)))

(export 'str.rev 'str.len 'str.int->str 'str.str->int)
    
;; Computes the length of the given string
(define str.len (str)
  (declare
   (i integer))
  (block
   (set i 0)
   (while (!= 0 (array str i))
    (set i (+ i 1)))

   (return i)))

;; Reverses the given string
(define str.rev (str)
 (declare
  (i integer)
  (j integer)
  (tmp byte))

 (block
  (set i 0)
  (set j (- (str.len str) 1))

  (while (> j i)
   (block
    (set tmp (array str i))
    (set (array str i) (array str j))
    (set (array str j) tmp)

    (set i (+ i 1))
    (set j (- j 1))))))

(define str.int->str (x buffer)
 (declare
  (index integer))

 (block
  (if (== x 0)
   ;; Trivial case - for 0, just put a 0 and close the buffer
   (block
    (set (array buffer 0) 48)
    (set (array buffer 1) 0))
   
   (block
    (set index 0)
    (while (> x 0)
     (block
      (set (array buffer index) (+ (% x 10) 48))
      (set index (+ index 1))
      (set x (/ x 10))))

    (set (array buffer index) 0)
    (str.rev buffer)))))

(define str.str->int (buffer)
 (declare
  (accum integer)
  (index string))

 (block
  (set index buffer)
  (set accum 0)

  (while (&& (>= (deref index) 48)
             (<= (deref index) 57))
   (block
    (set accum (* accum 10))
    (set accum 
     (+ accum (- (deref index) 48)))

    (set index (+ index 1))))

  (return accum)))
