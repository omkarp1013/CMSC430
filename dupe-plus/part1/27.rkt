#lang racket

(case (not 1)
    [(1 3 4) 1]
    [(5 6 7 8) 2]
    [(3 #f) 3]
    [else #f])