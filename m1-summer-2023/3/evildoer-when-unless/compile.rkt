#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax)
(define rsp 'rsp)

;; Expr -> Asm
(define (compile e)
  (prog (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Global 'entry)
        (Label 'entry)
        (Sub rsp 8)
        (compile-e e)
        (Add rsp 8)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Int i)        (compile-value i)]
    [(Bool b)       (compile-value b)]
    [(Char c)       (compile-value c)]
    [(Eof)          (compile-value eof)]
    [(Prim0 p)      (compile-prim0 p)]
    [(Prim1 p e)    (compile-prim1 p e)]
    [(When e1 e2)   (compile-when e1 e2)] ;; NEW
    [(Unless e1 e2) (compile-unless e1 e2)] ;; NEW
    [(If e1 e2 e3)  (compile-if e1 e2 e3)]
    [(Begin e1 e2)  (compile-begin e1 e2)]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Op0 -> Asm
(define (compile-prim0 p)
  (compile-op0 p))

;; Op1 Expr -> Asm
(define (compile-prim1 p e)
  (seq (compile-e e)
       (compile-op1 p)))

;; Expr Expr -> Asm
(define (compile-when e1 e2)
  (let ((e1-false (gensym 'when))
        (e2-true) (gensym 'true))
    (seq (compile-e e1)
        (Cmp rax (value->bits #f)
        (Je e1-false)
        (compile-e #t)
        (Jmp e1-true)
        (Label e1-false)
        (Mov rax (value->bits void))
        (Label e1-true)
        ))))

;; Expr Expr -> Asm
(define (compile-unless e1 e2)
  (let (e1-false (gensym 'unless))
       (e1-true (gensym 'unless))
    (seq (compile-e e1)
         (Cmp rax (value->bits #f)
         (Jne e1-true)
         (compile-e e2)
         (Jmp e1-false)
         (Label e1-true)
         (Mov rax (value->bits void)
         (Label e1-false))))))

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

;; Expr Expr -> Asm
(define (compile-begin e1 e2)
  (seq (compile-e e1)
       (compile-e e2)))
