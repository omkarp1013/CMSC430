#lang racket

(case 1
    [(2 3 4) 'first]
    [(5 6 7) 'second]
    [(0 (- 1) (abs (- 1)) #t #f) 'third]
    [(1 (- 2) (abs (- 2))) 'fourth]
    [else 'fifth])