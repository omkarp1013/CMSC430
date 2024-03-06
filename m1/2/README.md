# CMSC 430 Midterm 1, Part 2

## Instructions

You've been provided an incomplete modification to the Dupe language
implementation as presented in class.

The designers of this modified version of Dupe have decided to use a
different encoding of values than the one covered in class; they have
decided that:

- `#f` will be represented by `0`,
- `#t` will be represented by `1`,
- a negative integer `n` will be represented by itself (i.e. `n`), and
- a non-negative integer will be represented by `n+2`.

The run-time system (i.e. the C code) and the functions `value->bits`
and `bits->value` have been updated to use this new encoding, but none
of the rest of the code has been updated.

Make all the necessary changes to complete the modification to Dupe so
that it works correctly.

Once you've completed the new design, write a short description of the
pros and cons of this encoded compared to the one we went over in
class in pros-cons.md.
