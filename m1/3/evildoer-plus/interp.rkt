#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void
;; Expr -> Value
(define (interp e)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Prim0 p)
     (interp-prim0 p)]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(Prim2 p e1 e2)
     (interp-prim2 p (interp e1) (interp e2))]
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]
    [(Begin e1 e2)
     (begin (interp e1)
            (interp e2))]))

