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
;;  (pointer-to T) indicates a pointer to the type T.
;;  (array-of T N) allocates an array of size N, with elements of the type T.
;;  (ascii S) declares a string. Strings are delimited by ", C-style escapes
;;    allowed. Note that strings are not expressions, so you can't use them
;;    anywhere else but here.
;;  (func-pointer R P*) is a pointer to a function which takes the parameter
;;    types P and returns the type R.
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
;;  - Declaring string constants
;;  - Defining variables (via the other types)
;;
;; Import is like declare, but:
;;
;;  - It only works with functions and global values
;;  - The values/functions cannot be implemented in the current file - they 
;;    are marked as external
;;
;; Export is like import, but it allows other modules to use the functions
;; and values that it exports. Note that export must be after the declare 
;; block.
(declare
 (mars.sbrk
  (function (pointer-to byte) integer))

 (mars.read-int
  (function integer))

 (mars.print-int
  (function byte integer))

 (mars.print-string
  (function byte (pointer-to byte)))

 (linked-ints
  (struct (value integer)
          (next (pointer-to linked-ints))))

 (linked-ints-ptr
  (alias (pointer-to linked-ints)))

 (cons-linked-ints
  (function linked-ints-ptr
   integer linked-ints-ptr))

 (sum-linked-ints
  (function integer 
   linked-ints-ptr))

 (main
  (function integer)))

(import mars.sbrk mars.print-int mars.read-int mars.print-string)
(export cons-linked-ints sum-linked-ints)

;; Function definitions are different from C in four ways:
;;
;; - They don't contain type specifications (made redundant by forward decls)
;; - All functions must have a declare block before their bodies, even if it is
;;   empty
;; - They can't return structures directly (this alleviates some complications
;;   in the function call / return handling code)
;; - They can't take arrays as parameters, or return them, since there isn't a
;;   way to get a value of an array type, because they decay immediately
;;
;; Functions can also be defined with assembly code:
;;
;; (assemble NAME CODE-STRING)
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
 ;;
 ;; Getting the size of a type is accomplished via (size-of T)
 ;;
 ;; Truncate/expand bytes <-> integers:
 ;;  (byte-to-int E) and (int-to-byte E)
 (block
  (set list (cast linked-ints-ptr (mars.sbrk (size-of linked-ints))))
  (set (field (deref list) value) head)
  (set (field (deref list) next) tail)
  (return list)))

(define sum-linked-ints (list)
 (declare
  (sum integer))
 (block

  ;; Assignments take the form:
  ;;
  ;;  (set A E) updates assignable target A with the value in V.
  ;;  
  ;;  Assignable targets include (array ...), (field ...), (deref ...) as 
  ;;  well as  variables
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
  (while (!= list 0)
   (block
    ;; Accessors / assignable targets:
    ;;
    ;;   (array E I) gets the value at the pointer E with I units of offset.
    ;;     The C equivalent is e[i]
    ;;   (field E F+) gets the value of a structure E by traversing the fields F.
    ;;     The C equivalent is (e).f1.f2.f3.etc.fn
    ;;
    ;; Arithmetic (same as in C, however, there is no pointer arithmetic)
    ;;  + - * / % & | ~ << >> >>>
    ;;
    ;; Logical expressions:
    ;;   (&& E E) and (|| E E) are the short-circuiting versions of the
    ;;   bitwise operators & and |
    ;;   (! E) takes the logical negation of E
    (set sum (+ sum (field (deref list) value)))
    (set list (field (deref list) next))))

  ;; Returns take the form:
  ;;
  ;;  (return E) where E is an expression. Since void is not in this language,
  ;;    all returns must have values.
  (return sum)))

(define main ()
 (declare
  (list linked-ints-ptr)
  (read-value integer)
  (newline (array-of byte 2)))

 (block
  (set list (int-to-ptr linked-ints-ptr 0))
  (set read-value 1)
  
  (set (array newline 0) (int-to-byte 10))
  (set (array newline 1) (int-to-byte 0))

  (while (!= read-value 0)
   (block
    (set read-value (mars.read-int))
    (set list (cons-linked-ints read-value list))))

  (mars.print-int (sum-linked-ints list))
  (mars.print-string newline)))
