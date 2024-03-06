#lang racket
(provide compile-op1)
(require "ast.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define r9  'r9)

;; Op1 -> Asm
(define (compile-op1 p)
  (match p
    ['add1 
     (let ((neg-one (gensym 'add1))
          (pos (gensym 'add1)))
      (seq (Cmp rax -1)
           (Je neg-one)
           (Add rax 1)
           (Jmp pos)
           (Label neg-one)
           (Add rax 3)
           (Label pos)))]
    ['sub1 
      (let ((zero (gensym 'sub1))
           (not-zero (gensym 'sub1)))
        (seq (Cmp rax 2)
             (Je zero)
             (Sub rax 1)
             (Jmp not-zero)
             (Label zero)
             (Sub rax 1)
             (Sub rax 1)
             (Sub rax 1)
             (Label not-zero)))]
    ['zero?
     (seq (Cmp rax 2)
          (Mov rax (value->bits #f))
          (Mov r9  (value->bits #t))
          (Cmove rax r9))]))

