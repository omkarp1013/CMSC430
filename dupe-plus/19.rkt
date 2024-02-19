#lang racket

(cond
    [(zero? (add1 (- 1))) 1]
    [else #t])