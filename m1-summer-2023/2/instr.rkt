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
    ))