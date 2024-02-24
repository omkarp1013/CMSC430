#lang racket

(define lst '(1))

(match (rest lst)
    ['() #t])