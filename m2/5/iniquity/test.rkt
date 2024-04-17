#lang racket
(require "ast.rkt")
(require "parse.rkt")

(define p1 (parse-prog '(define (add x y) (+ x y))
                        '(define (sub x y) (- x y))
                      '(add 1 2)))
(define p2 (parse-prog '(define (sub x y) (- x y))
                      '(sub 3)))
(define p3 (parse-prog '(define (mul x y) (* x y))
                      '(mul 4 5)))
                    
(define p4 (parse-prog '(mul 4 5)))

(correct-arity? p1)
(correct-arity? p2)
(correct-arity? p4)