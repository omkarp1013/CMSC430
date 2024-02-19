#lang racket

(cond [(zero? 0) 1]
      [(zero? (- 1)) 1]
      [else #t])