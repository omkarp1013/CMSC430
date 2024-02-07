#lang racket
(require a86)

(asm-interp
 (prog (Global 'entry)
       (Label 'entry)
       (Mov 'rax 42)
       (Ret)))