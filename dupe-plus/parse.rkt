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
        [1 
          (match cs 
            [(list (list 'else exp)) (Cond '() (parse-cond-clauses cs))]
            [_ (error "Parse error")])]
        [_ (match (last cs)
            [(list 'else exp) (Cond (parse-cond-clauses (all-minus-last cs)) (parse-cond-clauses (list (last cs))))]
            [_ (error "Parse error")])])]  
    [(list 'case exp cs ...)
      (match (length cs)
        [0 (error "Parse error")]
        [1 (match cs 
            [(list 'else exp) (Case (parse exp) '() (parse-case-clauses cs))]
            [_ (error "Parse error")])]
        [_ (match (list (last cs))
            [(list 'else exp) (Case (parse exp) (parse-case-clauses (all-minus-last cs)) (parse-case-clauses (list (last cs))))]
            [_ (error "Parse error")])])]
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
        ['() (Clause (parse e1) (parse e2))]
        [(cons n xs) (list (Clause (parse e1) (parse e2)) (parse-cond-clauses (rest cs)))])]))
    
(define (all-minus-last lst)
  (reverse (rest (reverse lst))))

(define (parse-case-clauses cs)
  (match (first cs)
    [(list 'else exp) (parse exp)]
    [(list lst exp)
      (match (rest cs)
        ['() (if (int-bool lst) (Clause lst (parse exp)) (error "Parse error"))]
        [(cons n xs) (if (int-bool lst) (list (Clause lst (parse exp)) (parse-case-clauses (rest cs))) (error "Parse error"))])]))

(define (int-bool lst)
  (match lst
    ['() #t]
    [(cons n xs) (and (datum? n) (int-bool xs))]))



