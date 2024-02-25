#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define rdx 'rdx)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Label 'entry)
        (compile-e e)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d)         (compile-value d)]
    [(Prim1 p e)     (compile-prim1 p e)]
    [(Cond cs e)     (compile-cond cs e)]
    [(Case exp cs e) (compile-case exp cs e)]
    ;; TODO: Handle case
    [(If e1 e2 e3)
     (compile-if e1 e2 e3)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr Expr -> Asm
(define (compile-if e1 e2 e3)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2)
         (Jmp l2)
         (Label l1)
         (compile-e e3)
         (Label l2))))

(define (compile-cond cs e)
  (match cs
    ['() (compile-e e)]
    [(Clause p b)
      (let ((l1 (gensym 'cond))
            (l2 (gensym 'cond)))
        (seq (compile-e p)
             (Cmp rax (value->bits #f))
             (Je l1)
             (compile-e b)
             (Jmp l2)
             (Label l1)
             (compile-cond '() e)
             (Label l2)))]
    [(cons (Clause p b) xs)
      (let ((l3 (gensym 'cond))
            (l4 (gensym 'cond)))
        (seq (compile-e p)
             (Cmp rax (value->bits #f))
             (Je l3)
             (compile-e b)
             (Jmp l4)
             (Label l3)
             (compile-cond xs e)
             (Label l4)))]))

(define (compile-case exp cs e)
  (let ((end-label (gensym 'case)))
    (seq 
      (compile-e exp) ;; exp is in Rax
      (compile-case-lst cs end-label)
      (Label end-label))))

(define (compile-case-lst cs end-label)
  (match cs
    [(cons n xs)
      (seq (compile-case-clause n end-label)
           (compile-case-lst xs end-label))]
    ['() (seq )]))

(define (compile-case-clause c label)
  (match c
    [(Clause p b)
      (let ((false (gensym 'clause)))
           (compile-case-datum p false)
           (compile-e b)
           (Label false))]))

(define (compile-case-datum lst l1)
  (match lst
    ['() (seq (Jmp l1))]
    [(cons n xs)
      (seq (Push rax)
           (compile-e n) 
           (Mov rdx rax)
           (Pop rax)
           (Cmp rax rdx)
           (Jne l1)
           (compile-case-datum xs l1))]))





