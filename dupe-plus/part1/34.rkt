#lang racket

(case (cond [(not #t) 1]
      [1 2]
      [else 3])
      
      [(3 4) 1]
      [(1) 2]
      [else 3])