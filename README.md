Scheme Interpreter in Haskell
=============================

This project is based on Jonathan Tang's [tutorial][], mirrored on
[Wikibooks][]. The goal is to write an interpreter in Haskell that implements
a subset of [R5RS][] Scheme.

I've also worked through some of the exercises, and split the Haskell file up
into separate modules. In the future, depending on time and interest, I may
continue adding more of R5RS Scheme (or [R6RS][] Scheme) and portions of the
standard library.

Usage
-----

You will need [Haskell][].

To compile the interpreter:

    $ ghc --make scheme.hs

To run a Scheme program:

    $ ./scheme factorial.scm
    120

To use the REPL:

    $ ./scheme
    Scheme>>> (load "stdlib.scm")
    Scheme>>> (sum 1 2 3 4 5)
    15

Exit with CTRL-D.

[tutorial]: http://jonathan.tang.name/files/scheme_in_48/tutorial/overview.html
[Wikibooks]: http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours
[R5RS]: http://www.schemers.org/Documents/Standards/R5RS/HTML/
[R6RS]: http://www.r6rs.org/
[Haskell]: http://www.haskell.org/platform/
