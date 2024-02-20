#lang racket

(cond
    [(zero? (add1 (abs (- 1)))) 1]
    [(not #f) 2]
    [else (add1 (abs (- 1)))])