#lang racket
(require "ast.rkt")
(require "parse.rkt")

(parse '(case (cond [(not #t) 1]
      [1 2]
      [else 3])
      
      [(3 4) 1]
      [(1) 2]
      [else 3]))

