#lang racket

(case (+ 1 2)
    [(1 2 4) 'first]
    [(5 6 #t) 'second]
    [(#t #f 3) 'third]
    [else 'fourth])