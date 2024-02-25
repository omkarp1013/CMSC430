#lang racket
(require "ast.rkt")
(require "parse.rkt")


(define e (parse '(cond [(cond [else #f]) 1] [(not #t) #f]
      [else #t])))

(match e
      ['() #f]
      [(Cond cs e)
            (match cs
                  [(cons n xs) n])])

