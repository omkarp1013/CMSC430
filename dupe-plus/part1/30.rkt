#lang racket

(case (cond
    [(zero? (add1 (abs (- 1)))) 1]
    [else (not #f)])

    [(1 2) (add1 0)]
    [(3 4) (add1 1)]
    [(#f #t) (add1 2)]
    [else (add1 3)]
)