#lang racket
(require "ast.rkt")
(require "parse.rkt")



(define e (parse '(case 2 [() 3] [else 1])))

(match e
      ['() #f]
      [(Case exp cs el)
            (match cs
                  [(Clause p b) p]
                  [(cons n xs) n])])

