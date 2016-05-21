;; Comments start with ; and go to the end of the line
;;
;; Primitive types:
;;   integer
;;   byte
;;   string (aliased to (pointer-to byte))
;;
;; Note an important omission for C programmers - void*. This complicates
;; a few things (using pointers as arrays, mostly), without much benefit,
;; since pointers can always be cast. The idea is to use (pointer-to byte)
;; as a replacement, like C programs used to use char*.
;;
;; Also, there are no floating point operations (yet).
;;
;; Void is missing since it is not terribly useful, and complicates return
;; handling for no real benefit - just return a 'byte'.
;;
;; General combined types:
;;  (pointer-to T) where T is a type
;;  (array-of T N) where T is a type and N is a positive integer
;;    Note that array-of immediately decays into a pointer.
;;  (func-pointer R T*) where R is the return type and T is a parameter type.
;;
;; Types which can appear only in declarations:
;;  (function R T*) where R is a type (the return type)
;;                    and T are the parameter types
;;  (struct (I T)+) where I is a field identifier and T is a type
;;  (alias T) where T is the type to alias
;;
;; Builtin functions for the MARS version (on top of system calls):
;;
;; (declare
;;   (@print-int (function byte integer))
;;   (@print-string (function byte (pointer-to byte)))
;;   (@read-int (function integer))
;;   (@read-string (function byte (pointer-to byte) integer))
;;   (@sbrk (function (pointer-to byte) integer))
;;   (@exit (function byte)))
;;
;; Declare is a general purpose forward-declaration facility. It is
;; responsible for:
;;
;;  - Defining structures via struct
;;  - Binding type aliases via alias
;;  - Declaring function signatures via function
;;  - Defining variables (via the other types)
    
(declare
 (linked-ints
  (struct (value integer)
          (next (pointer-to linked-ints))))

 (linked-ints-ptr
  (alias linked-ints))

 (cons-linked-ints
  (function linked-ints-ptr
   integer linked-ints-ptr))

 (sum-linked-ints
  (function integer 
   linked-ints-ptr))

 (main
  (function integer))

 (global-var integer))

;; Function definitions are different from C in two ways:
;;
;; - They don't contain type specifications (made redundant by forward decls)
;; - All functions must have a declare block before their bodies, even if it is
;;   empty
(define cons-linked-ints (head tail)
 (declare
  (list linked-ints-ptr))

 ;; Pointers:
 ;;
 ;;  (ref E) gets a pointer to the value produced by E.
 ;;  (deref E) gets the value pointed to by the pointer produced by E
 ;;  (ptr-to-int E) gets the pointer which has the integer in E as its address
 ;;  (int-to-ptr E T) gets the integer address in the pointer produced by E
 ;;  (cast T E) casts the pointer produced by E to another type T
 ;;  (func I) gets a function pointer which is bound to the function of the
 ;;    given name.
 ;;
 ;; The need for (func I) stems from wanting to keep the namespaces of types
 ;; and values separate (as in C) as well as keeping the namespace of functions
 ;; separate from both of those (as in Common Lisp).
 ;;
 ;; Getting the size of a type is accomplished via (size-of T)
 (block
  (set list (ptr-to-int (@sbrk (size-of linked-ints))))
  (set (field list value) head)
  (set (field list rest) tail)
  (return list)))

(define sum-linked-ints (list)
 (declare
  (sum integer))
 (block

  ;; Assignments take the form:
  ;;
  ;;  (set A E) updates assignable target A with the value in V.
  ;;
  (set sum 0)

  ;; Comparison operators: == != < > <= >=
  ;; Coercion: Pointers are coerced to their integer addresses.
  ;; 
  ;; Loops take the form:
  ;;
  ;;   (while E B) is a standard while loop with E as the condition and B as
  ;;     the body
  ;;   (break) and (continue) work as in C, and are allowed only in
  ;;     loops
  ;;
  ;; Conditionals take the form:
  ;;   (if E B B?) where E is the condition, and the B's are the true body
  ;;     and the optional false body
  (while (!= list null)
   (block
    ;; Accessors / assignable targets:
    ;;
    ;;   (array E I) gets the value at the pointer E with I units of offset.
    ;;     The C equivalent is e[i]
    ;;   (field E F+) gets the value of a structure E by traversing the fields F.
    ;;     The C equivalent is (e).f1.f2.f3.etc.fn
    ;;
    ;; Arithmetic (do the same as in C)
    ;;  + - * / % << >>
    (set sum (+ sum (field list value)))
    (set list (field list next))))

  ;; Returns take the form:
  ;;
  ;;  (return E) where E is an expression. Since void is not in this language,
  ;;    all returns must have values.
  (return sum)))

;; The main function is always called with the standard C-like argument pair.
(define main (argc argv)
 (declare
  (list linked-ints-ptr)
  (read-value integer)
  (newline (array-of byte 2)))

 (block
  (set list (int-to-ptr 0 linked-ints-ptr))
  (set read-value 1)
  
  (set (array newline 0) 10)
  (set (array newline 1) 0)

  (while (!= read-value 0)
   (block
    (set read-value (@read-int))
    (set list (cons-linked-ints read-value list))))

  (print-int (sum-linked-ints list))
  (print-string newline)))
