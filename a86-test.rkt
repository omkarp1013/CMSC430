#lang racket
(require a86)

(asm-interp
 (prog (Global 'entry)
       (Label 'entry)
       (Mov 'rax 9) ; 
       (Or 'rax 1)
       (Ret)))