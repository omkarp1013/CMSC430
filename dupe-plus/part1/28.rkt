#lang racket

(case (cond [(not #t) 1]
      [else #f])

      [(1 2 #t) 1]
      [(3 4 #f) 2]
      [else 3])