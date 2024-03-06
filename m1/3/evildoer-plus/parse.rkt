#lang racket
(provide parse)
(require "ast.rkt")

;; S-Expr -> Expr
(define (parse s)
  (match s
    ['eof                (Eof)]
    [(? datum?)          (Lit s)]
    [(list (? op0? o))   (Prim0 o)]
    [(list (? op1? o) e) (Prim1 o (parse e))]
    [(list (? op2? o) e1 e2) (Prim2 o (parse e1) (parse e2))]
    [(list 'begin e1 e2) (Begin (parse e1) (parse e2))]
    [(list 'if e1 e2 e3)
     (If (parse e1) (parse e2) (parse e3))]
    [_ (error "Parse error")]))


;; Any -> Boolean
(define (datum? x)
  (or (and (exact-integer? x)
           (<= (- (expt 2 62))
               x
               (sub1 (expt 2 62))))
      (boolean? x)
      (char? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?)))

(define (op2? x)
  (memq x '(+)))
