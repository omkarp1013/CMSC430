#lang racket
(require "parse.rkt"
         "interp.rkt"
         "compile.rkt"
         "types.rkt"
         a86/interp)



(bits->value (asm-interp (compile (parse '(case #t
    [(1 2) (add1 0)]
    [(3 4) (add1 1)]
    [(#f #t) (add1 2)]
    [else (add1 3)])))))

