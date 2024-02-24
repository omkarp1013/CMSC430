#lang racket

(case (cond
    [(zero? (add1 (abs (- 1)))) 1]
    [(not #f) 2]
    [else (add1 (abs (- 1)))])
    
    ['() 1]
    [(2 4) 2]
    [else 3]
    )