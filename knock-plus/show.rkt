#lang racket
(require "compile.rkt")
(require "parse.rkt")
(require racket/pretty)



(pretty-print (compile (parse '(match (vector 1 2 3)
                          [(vector 1 1 3) 1]
                          [x x]))))