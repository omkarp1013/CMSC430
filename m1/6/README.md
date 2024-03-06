# CMSC 430 Midterm 1, Part 6

## Instructions

In our discussion of Fraud, we observed that it's possible to look at
an expression and know a lot about the structure of the environments
that will be used when the expression is interpreted.

For example, when evaluating the variable occurrence `x` in:
```
(let ((p 1))
  (let ((x 2))
    (let ((q 3))
	  x)))
```
we know that the environment will consist of three bindings: one for
`p`, one for `x`, and one for `q`.

For this problem, you've been given the AST definition for Fraud and
you are asked to write a function `max-env-length` that computes the
length of the longest environment that will be used when interpreting
the expression.

For example,

- the max environment length of `(Lit 5)` is 0,
- the max environment length of `(Let 'x (Lit 5) (Var 'x))` is 1.
- the max environment length of 
```
(Prim2 '+ (Let 'x (Lit 5) (Var 'x)) (Let 'y (Lit 6) (Var 'y)))
```
is 1.
- the max environment length of
```
(Let 'x (Lit 5) (Let 'y (Lit 6) (Var 'y)))
```
is 2.

Update the code for `max-env-length` in `ast.rkt` to implement
this function.

