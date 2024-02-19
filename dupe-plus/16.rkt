#lang racket

(cond [(zero? (abs (- 1))) 1]
      [(zero? 0) 2]
      [else 3])