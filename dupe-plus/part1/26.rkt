#lang racket

(case (not #t)
    [(1 3 4) 1]
    [(5 6 7 8) 2]
    [(3 #f) 3]
    [else #f])