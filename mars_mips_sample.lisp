;; Comments start with ; and go to the end of the line
;;
;; Primitive types:
;;   integer
;;   byte
;;   string (aliased to (pointer-to byte))
;;
;; There are no floating point types.
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
;; Note an important omission for C programmers - void*. This complicates
;; a few things (using pointers as arrays, mostly), without much benefit,
;; since pointers can always be cast. The idea is to use (pointer-to byte)
;; as a replacement, like C programs used to use char*.
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
;; There are two ways to interface with other files - export and require
;;  - (require STRING) reads in another file, and loads the definitions that the other
;;    file exports
;;  - (export N*) declares that the names N are available to other files. They must be
;;    prefixed with either ' (representing values) or * (representing types)
;;
;; Finally, spc supports a kind of static metaprogramming - static if, written *if. It
;; is used to conditionally compile code:
;;
;;    (*if COND C1 C2?)
;;
;; Depending upon the value of COND (a compile-time conditional), this expands to either
;; C1 or C2 in-place (or nothing if C2 is not givenm, and COND is false). Currently, the
;; following conditions are supported:
;;
;;    STRING - Always true
;;    INTEGER - True when the value is != 0
;;    (platform? STRING STRING) - True when the backend's OS and platform are 
;;        the same as the given strings. Note that either may be the wildcard "*"
;;    (var-def? IDENTIFIER) - True when the given variable is in scope, False otherwise.
;;    (type-def? IDENTIFIER) - True when the given type is in scope, False otherwise.
;;
;; In conjunction with *if, *error allows users to raise compile-time errors from within
;; their own code:
;;
;;    (*error STRING)

(*if (platform? "mars" "mips")
 (require "arch/mars_mips.lisp")
 (*error "Sample must be compiled with 'mars_mips' backend"))

(declare
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

(export *linked-ints *linked-ints-ptr 'cons-linked-ints 'sum-linked-ints)

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
 ;;  (cast T E) casts E to the type T. This works for:
 ;;
 ;;    - byte -> pointer
 ;;    - int <-> pointer
 ;;    - pointer <-> pointer
 ;;    - int <-> byte
 ;;
 ;; Getting the size of a type is accomplished via (size-of T)
 ;; 
 ;; However, note that integers are automatically downcast, and bytes
 ;; automatically upcast, when a value of one type or the other is needed.
 ;; Thus, these functions generally do not need to be used.
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
  ;;
  ;;   (switch
  ;;    (case E1 B1)
  ;;    (case E2 B2)
  ;;    (case E3 B3)
  ;;    ...
  ;;    (else Bn))
  ;;
  ;; Switch is closer to Scheme's cond than C-style switch.
  (while (!= list 0)
   (block
    ;; Accessors / assignable targets:
    ;;
    ;;   (array E I) gets the value at the pointer E with I units of offset.
    ;;     The C equivalent is e[i]
    ;;   (field E F+) gets the value of a structure E by traversing the fields F.
    ;;     The C equivalent is (e).f1.f2.f3.etc.fn
    ;;
    ;; Arithmetic (same as in C, pointer arithmetic included for + and -)
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
  (set list (cast linked-ints-ptr 0))
  (set read-value 1)
  
  (set (array newline 0) 10)
  (set (array newline 1) 0)

  (while (!= read-value 0)
   (block
    (set read-value (mars.read-int))
    (set list (cons-linked-ints read-value list))))

  (mars.print-int (sum-linked-ints list))
  (mars.print-string newline)))
