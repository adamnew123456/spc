The Some-Pass Compiler
======================

This is a compiler for a language which is something like simple Pascal, but
with a Lisp-like syntax. It is written in a direct, 2.5 pass style (the two
passes are lexing and compilation; the extra half comes from the fact that
expressions are compiled from a syntax tree, though nothing else is).
There are no ASTs nor IRs in site - not long after a statement is read, the
compiler writes the code for it, and moves onto the next.

It is only of interest for educational purposes, since it is:

- Non-optimizing: Excess loads/stores galore. Writing to a register only to copy
  it to another in the next line. Dumping temporary values to the stack, rather
  than registers. It produces functioning code, but nowhere near the best.
- Dense: Currently, it isn't all that well factored - there are lots of parts 
  which could be cleaned up and separated from the backend. As it stands, all
  of the stuff that isn't verifying that the syntax is good is left up to the
  specific target backend.
- Single Target: This could be expanded, but it isn't all that important of a
  goal. Right now, the only target is the MARS simulator, and even then, it's
  somewhat of a bad citizen (not following the calling conventions fully is
  the biggest problem, although there's a reason)
- Single File: The language has no idea about linkage, so it's difficult to
  share code across files without hacking in extern directives yourself. Whether
  I fix this is a tossup - I want to add namespaces, and if I can find the time
  to do them, I'll probably fix this issue then as well.
- Error Reporting: Most errors use the line and column number 0, 0 because
  the driver doesn't inform the backend of locations in the file. This is
  one of the higher priority things to fix.

Other than that, it produces code for a language which is pretty much on the
same abstractive level as C - but, the language is organized explicitly to be
easier to compile than standard C.

The Language
============

See ``sample.lisp`` - it contains many comments detailing the language's builtin
types and functions, notes about the restrictions which make the compiler somewhat
easier to implement. As a bonus, it actually serves as a runnable example which
computes the sum of the numbers entered::

    $ python3 compile.py -b mars -o sample.asm sample.lisp
    $ java -jar Mars.jar sample.asm
    5
    10
    15
    0
    30

Compiling Things
================

Run ``python3 compiler.py -h`` to see the compiler's help. The only backend
right now is ``mars``.

Interesting Things
==================

In no particular order, things that writing this helped me understand.

- *The lvalue/rvalue Distinction.* I originally was going to have three special
  assignment forms - one for variables, one for fields, and one for arrays
  (I unintentionally ignored dereference assignments like ``*x = 5``). When it
  came time to implement them, I realized that I could slot these into the
  general expression evaluation mechanism by having *ref* loads which return
  addresses of values, and *val* loads which return actual values. By assigning
  to the address in a *ref* load, the compiler would assign to the value backing
  the expression in memory. Replace *ref* with *lvalue* and *val* with *rvalue*,
  and you have the way that C works. After I realized the unity of *ref* loads,
  I got rid of the separate assignment statements in favor of the unified
  assignment the compiler (and the C language) uses today.