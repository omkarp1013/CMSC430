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
    [(list 'cond cs e) (match cs
                              ['() (Cond '() (parse e))]
                              [(cons (list ) xs) (Cond (list (Clause (parse n) (parse xs)) (parse e))])]   
    [(list 'case e cs el) (match cs
                              ['() (Case (parse e) '() (parse el))]
                              [(cons n xs) (Case (parse e) (list Clause (parse n)))])]
    ;; TODO: Handle case
    ;; TODO: Remove this clause once you've added clauses for
    ;; parsing cond and case; it's here just so running the test suite
    ;; doesn't trigger parse errors.
    [_ (Lit 0)]
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

(define )


