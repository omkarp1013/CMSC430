#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define r9  'r9)  ; scratch
(define rsp 'rsp) ; stack

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Label 'entry)
        (Sub rsp 8)
        (compile-e e)
        (Add rsp 8)
        (Ret)))

;; Expr -> Asm
(define (compile-e e)
  (match e
    [(Lit d) (compile-value d)]
    [(Eof) (compile-value eof)]
    [(Prim0 p) (compile-prim0 p)]
    [(Prim1 p e) (compile-prim1 p e)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2)]    
    [(If e1 e2 e3)
     (compile-if e1 e2 e3)]
    [(Begin e1 e2)
     (compile-begin e1 e2)]))

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

;; Op2 Expr Expr -> Asm
(define (compile-prim2 p e1 e2)
  (seq (compile-e e1)
       (Push rax)
       (compile-e e2)       
       (Mov r9 rax)
       (Pop rax)
       (match p
         ['+ (Add rax r9)])))       

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

