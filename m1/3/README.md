# CMSC 430 Midterm 1, Part 3

## Instructions

You've been provided a modified implementation of Evildoer.

The parser, interpreter, and compiler have been updated to
include support for a single binary operation:

- `(+ e1 e2)`

The compiler emits code to do the following for `(+ e1 e2)`:

- execute the instructions for `e1`
- push the value in `rax` on to the stack
- execute the instructions for `e2`
- move the value in `rax` into `r9`
- pop the value on the stack into `rax`
- add `rax` and `r9`, using `rax` as the destination register

You can see the code for this in the `compile-prim2` function inside
of `compile.rkt`.

There are tests included in the `test/` directory.

Everything seems to be working just fine!  However, this modification
has introduced a bug into the compiler.

Write a program in `bug.rkt` that uses the new `+` operation and
demonstrates a bug in the compiler (i.e. the interpreter and compiler
should produce different results).

Note: You do not need to modify the given code at all -- you simply
need to write a program that demonstrates a bug in the compiler you
are given.  Your bug must involve using `+` and the interpreter has
been carefully written so that integer overflow and underflow are
unspecified behavior.

Hint: think about how this new code violates some assumptions made in
the other parts of the compiler.
