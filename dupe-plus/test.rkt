#lang racket
(require "ast.rkt")
(require "parse.rkt")

(define s (parse '(case 2 [(1) 1] [(1 2 #f) 2] [else 1])))

(match s
  [(Case exp c el) (match c
                      [(cons n xs) (match n
                                      [(Clause p b) b])])])
    
