#lang racket
(require a86)
(provide is)

(define is
  (seq
    (Sar 'rax 1)
    (Sar 'rbx 1)
    (Sub 'rax 'rbx)
    (Sal 'rax 1)
    (Add 'rax 1)
    ;; Assume encoding of n1 in rax
    ;; and encoding of n2 in rbx

    ;; TODO: write instructions that produce
    ;; encoding of (- n1 n2) in rax
    ))