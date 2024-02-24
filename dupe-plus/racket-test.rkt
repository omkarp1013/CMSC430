#lang racket

(case (+ 1 2)
    [(1 2 4) 1]
    [(5 6 #t) 2]
    [(#t #f 3) 3]
    [else 4])