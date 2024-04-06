#lang racket
(require "ast.rkt")
(require "parse.rkt")

;; 108
(parse '(let ((x (+ 1 2)))
                          (let ((z (- 4 x)))
                            (+ (+ x x) z))))

;; 130
(parse '(let ((x 1) (y 2)) (+ x y)))


