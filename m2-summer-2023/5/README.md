# CMSC 430 Midterm 2, Part 5

## Instructions

For this problem, you are tasked with implementing a new unary
operation called `range`.  When given a non-negative integer `n`, it
produces a list of length `n` that counts up from zero to `n`-1.

For example:
```
> (range 5)
'(0 1 2 3 4)
```

When given a negative integer, it produces the empty list.  When
given any other value, it signals a run-time error.

You are given a modified version of Iniquity that has added `range` to
the parser and stubbed out `range` in the compiler and interpreter.
It has a few test cases included.  Complete the implementation of
`range`.
