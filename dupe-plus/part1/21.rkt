#lang racket

(cond [(cond
    [(zero? (add1 (abs (- 1))))]
    [else #f])]
    [else #t])