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
        [1 (Cond '() (parse-case-clauses cs))]
        [_ (Cond (parse-case-clauses (all-minus-last cs)) (parse-cond-clauses (list (last cs))))])]  
    [(list 'case exp cs ...)
      (match (length cs)
        [1 (Case (parse exp) '() (parse-cond-clauses cs))]
        [_ (Case (parse exp) (parse--cond-clauses (all-minus-last cs)) (parse-cond-clauses (list (last cs))))])]
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

(define (parse-cond-clauses cs)
  (match (first cs)
    [(list 'else exp) (parse exp)]
    [(list e1 e2)
      (match (rest cs)
        ['() (Clause e1 e2)]
        [(cons n xs) (list (Clause e1 e2) (parse-cond-clauses (rest cs)))])]))
    
(define (all-minus-last lst)
  (reverse (rest (reverse lst))))

(define (parse-case-clauses cs)
  (match (first cs)
    [(list 'else exp) (parse exp)]
    [(list (list ? ) exp)]))

