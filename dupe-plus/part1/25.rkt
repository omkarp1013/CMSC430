#lang racket

(case (add1 (abs (- 1)) 1)
    [(1 3 4) 1]
    [(5 6 7 8) 2]
    [(3) 3]
    [else #f])