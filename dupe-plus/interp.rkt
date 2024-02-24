#lang racket
(provide interp)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Value =
;; | Integer
;; | Boolean

;; Expr -> Value
(define (interp e)
  (match e
    [(Lit d) d]
    [(Prim1 p e)
     (interp-prim1 p (interp e))]
    [(Cond cs e) (interp-cond-clauses cs e)]
    [(Case e cs el) (interp-case-clauses (interp e) cs el)]   
    [(If e1 e2 e3)
     (if (interp e1)
         (interp e2)
         (interp e3))]))


(define (interp-cond-clauses cs e)
  (match cs
    ['() (interp e)]
    [(Clause p b)
      (match (interp p)
        [#f (interp e)]
        [_ (interp b)])]
    [(list (Clause p b) xs)
      (match (interp p)
        [#f (interp-cond-clauses xs e)]
        [_ (interp b)])]))

(define (interp-case-clauses v cs el)
  (match cs
    ['() (interp el)]
    [(Clause p b)
      (if (member v p) (interp b) (interp el))]
    [(list (Clause p b) xs)
      (if (member v p) (interp b) (interp-case-clauses v xs el))]))


      