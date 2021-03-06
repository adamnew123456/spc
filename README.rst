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

Other than that, it produces code for a language which is pretty much on the
same abstractive level as C - but, the language is organized explicitly to be
easier to compile than standard C.

The Language
============

See ``sample.lisp`` - it contains many comments detailing the language's builtin
types and functions, notes about the restrictions which make the compiler somewhat
easier to implement. As a bonus, it actually serves as a runnable example which
computes the sum of the numbers entered::

    $ python3 build.py mars_mips_sample.lisp
    $ cd build/mars_mips
    $ java -jar Mars.jar nc p sample.asm
    5
    10
    15
    0
    30

Compiling Things
================

Run ``python3 build.py`` to see the compiler's help. Currently, there
two backends:

- ``mars_mips``, for the MARS educational MIPS simulator.
- ``linux_x86``, for Linux running on 32-bit Intel processors. This has been
  tested with the GNU ``as`` assembler and the GNU ``ld`` linker.

All the samples that come with the compiler can be compiled via ``build.py``::

    $ python3 build.py samples/mars_mips/fizzbuzz.lisp
    $ cd build/mars_mips
    $ java -jar Mars.jar nc p fizzbuzz.asm
    1
    2
    Fizz
    4
    Buzz
    ...

    $ python3 build.py samples/linux_x86/fizzbuzz.lisp
    $ cd build/linux_x86
    $ ./fizzbuzz
    1
    2
    Fizz
    4
    Buzz
    ...

Running The Tests
=================

To ensure that the compiled programs behave in the expected way, use the
testing script::

    $ python3 test.py >& LOG 
    $ grep @fail LOG

If this command returns any results, then there is a bug somewhere (either in
the tests, or in the compiler), which can be understood by reading the testing
log.

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

- *Static If.* Having code that was backend independent was not possible for a
  good while, because of the lack of conditional compilation - this is because
  it was not possible to do conditional compilation based upon the backend in
  use. Rather than doing a separate macro system (like cpp) which was much more
  powerful a feature than simple conditional compilation warrants, I opted to 
  try doing static if, which I had been looking into at the time. The first 
  condition supported was *platform?*, which was sufficient to pull out a 
  small standard library and merge almost all the samples from both backends.
