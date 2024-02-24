#lang racket

(case (cond [(cond
    [(zero? (add1 (abs (- 1)))) 1]
    [else #f])]
    [else #t])

    [(2 3 4) 1]
    [(5 6 7) 2]
    [(0 10 11 12 13) 3]
    [(1 3 5 7) 4]
    [else (cond [(not #t) 1]
      [else #f])])