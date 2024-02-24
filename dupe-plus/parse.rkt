#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    [(? datum?)          (Lit s)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [(list 'cond cs ...) 
      (match (length cs)
        [1 (Cond '() (parse-clauses cs))]
        [_ (Cond (parse-clauses (all-minus-last cs)) (parse-clauses (last cs)))])]  
    ;; implement Case
    [_ (error "Parse error")]))

;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)))

(define (op1? x)
  (memq x '(add1
            sub1
            zero?
            abs
            -
            not)))

(define (parse-clauses cs)
  (match (first cs)
    [['else exp] (parse exp)]
    [(list e1 e2) (list (Clause e1 e2) (parse-clauses (rest cs)))]))

(define (all-minus-last lst)
  (reverse (rest (reverse lst))))