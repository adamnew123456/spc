(namespace str)
(declare 
 (rev (function byte string))
 (len (function integer string))
 (int->str (function byte integer string))
 (str->int (function integer string)))

(export 'rev 'len 'int->str 'str->int)
    
;; Computes the length of the given string
(define len (str)
  (declare
   (i integer))
  (block
   (set i 0)
   (while (!= 0 (array str i))
    (set i (+ i 1)))

   (return i)))

;; Reverses the given string
(define rev (str)
 (declare
  (i integer)
  (j integer)
  (tmp byte))

 (block
  (set i 0)
  (set j (- (len str) 1))

  (while (> j i)
   (block
    (set tmp (array str i))
    (set (array str i) (array str j))
    (set (array str j) tmp)

    (set i (+ i 1))
    (set j (- j 1))))))

(define int->str (x buffer)
 (declare
  (index integer))

 (block
  (if (== x 0)
   ;; Trivial case - for 0, just put a 0 and close the buffer
   (block
    (set (array buffer 0) #0)
    (set (array buffer 1) #\0))
   
   (block
    (set index 0)
    (while (> x 0)
     (block
      (set (array buffer index) (+ (% x 10) 48))
      (set index (+ index 1))
      (set x (/ x 10))))

    (set (array buffer index) 0)
    (rev buffer)))))

(define str->int (buffer)
 (declare
  (accum integer)
  (index string))

 (block
  (set index buffer)
  (set accum 0)

  (while (&& (>= (deref index) #0)
             (<= (deref index) #9))
   (block
    (set accum (* accum 10))
    (set accum 
     (+ accum (- (deref index) #0)))

    (set index (+ index 1))))

  (return accum)))
