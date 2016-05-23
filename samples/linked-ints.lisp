(declare
 (linked-ints
  (struct (value integer)
          (next (pointer-to linked-ints))))

 (linked-ints-ptr
  (alias (pointer-to linked-ints))))
