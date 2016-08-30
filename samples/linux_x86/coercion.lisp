;; This is meant to demonstrate all the constructs where automatic coercion 
;; can occur:
;;
;;
;;  1. Function argument values
;;  2. Return values (specifically, making sure that the x in (return x)
;;                    has the right type; promoting the result of functions
;;                    is implicit in other rules)
;;  3. Assignment (from a byte value->int variable and the other way around)
;;  4. Array indexes
;;  5. Arithmetic expressions
;;  6. Bitwise operations
;;  7. Logical expressions
;;  8. Comparison operations
;;  9. if/while/switch conditionals
;; 10. cast from byte to pointer
(namespace coercion-test)
(declare
 (int->byte (function byte integer))
 (byte->int (function integer byte))
 (main (function byte)))

(define int->byte (in)
 (declare)
 (return in))

(define byte->int (in)
 (declare)
 (return in))

(define main ()
 (declare
  (intval integer)
  (byteval byte)
  (bytearray (array-of byte 5))
  (byteptr (pointer-to byte)))

 (block
  ;; #3
  (set byteval 42) ;; int to byte
  (set intval byteval) ;; byte to int

  ;; #1 and #2 are tested by int->byte and byte->int - we force the input value
  ;; to be coerced, while the function forces its return value to be coerced
  (set byteval (int->byte byteval))
  (set intval (byte->int intval))

  ;; #4
  (set byteval 2)
  (set byteval (array bytearray byteval))

  ;; #5 (all share the same setup code, so no point in doing more than one)
  (set intval (+ byteval byteval))
  (set intval (+ byteval intval))
  (set intval (+ intval byteval))

  ;; #6 (see #5 note; not is different since it is unary)
  (set intval (& byteval byteval))
  (set intval (& byteval intval))
  (set intval (& intval byteval))

  (set intval (~ byteval))

  ;; #7 (see #5 note; not is different again, since it is unary)
  (set intval (&& byteval byteval))
  (set intval (&& byteval intval))
  (set intval (&& intval byteval))

  (set intval (! byteval))

  ;; #8
  (set intval (== byteval byteval))
  (set intval (== byteval intval))
  (set intval (== intval byteval))

  ;; #9
  (set byteval 0)

  (if byteval 0)
  (while byteval 0)
  (switch
   (case byteval 0))

  ;; #10
  (set byteptr (cast (pointer-to byte) byteval))))
