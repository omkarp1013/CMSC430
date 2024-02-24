#lang racket
(require "ast.rkt")
(require "parse.rkt")
(require "interp.rkt")

(interp (parse '(case (cond [(not #t) 1]
      [else #f])

      [(1 2 #t) 1]
      [(3 4 #f) 2]
      [else 3])))
