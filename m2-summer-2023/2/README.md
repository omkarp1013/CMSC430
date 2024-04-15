# CMSC 430 Midterm 2, Part 2

## Instructions

We have aimed to write ***correct*** compilers, where we view the
interpreter as specifying what correct means.  If the interpreter
produces some value for a given expression, running the code produced
by the compiler for that same expression should produce the same
value.  But almost from the beginning, we have had a lurking bug in
our compilers due to the fact that can only accommodate integers of
some fixed size, whereas our interpreters can handle arbitrarily large
integers.

When a specification and implementation are out of sync, we can
resolve the problem by changing the implementation or by changing the
spec (or both).  Ideally, we'd like to build a compiler that can
handle arbitrarily large integers, but that's a little much for an
exam question.  Instead, we have provided a modified version of the
Fraud language that has an interpreter that signals an error whenever
integers are produced that exceed the limits of what can be
represented in the compiler's representation of integers.

Your job is to update the compiler to reflect this new specification.

A few tests have been added to the `tests/` directory.  You can run
`raco test test/interp.rkt` to test the interpreter (all tests should
pass) and `raco test test/compile.rkt` to test the compiler (some
tests will fail).

Update the compiler so that it is correct with respect to the given
interpreter.
