#lang racket
(require "parse.rkt")

(parse '(define f (case-lambda))
        '(f))

(parse '(define f (case-lambda))
                    '(add1 8))