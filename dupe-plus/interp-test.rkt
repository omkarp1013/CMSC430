#lang racket
(require "ast.rkt")
(require "parse.rkt")
(require "interp.rkt")

(interp (parse '(cond [#f 1]
      [(case (cond [(not #t) 1]
      [1 (add1 1)]
      [else (add1 2)])
      
      [(3 4) 1]
      [(1) 2]
      [else 3]) (add1 1)]
      [else (add1 2)])))