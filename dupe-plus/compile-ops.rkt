#lang racket
(provide compile-op1)
(require "ast.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define rdx 'rdx)
(define r9  'r9)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 (seq (Add rax (value->bits 1)))]
    ['sub1 (seq (Sub rax (value->bits 1)))]
    ['abs  (let ((l1 (gensym 'abs)))
            (seq (Cmp rax (value->bits 0))
                 (Jge l1)
                 (Mov rdx rax)
                 (Sub rax rdx)
                 (Sub rax rdx)
                 (Label l1)))]
    ['-    (seq (Mov rdx rax)
                (Sub rax rdx)
                (Sub rax rdx))]
    ['not  (seq (Cmp rax (value->bits #f))
                (Mov rax (value->bits #f))
                (Mov r9 (value->bits #t))
                (Cmove rax r9))]
    ['zero?
     (seq (Cmp rax 0)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]))
