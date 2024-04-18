#lang racket

;; TODO: replace with an expression that triggers the bug in `cons`
;; When interpreted, it should produce 1,
;; but when compiled it should produce 2.

(let ([x (cons 1 2)]) (begin (let ([y (cons 3 2)]) (cons 1 2)) (let ([z (cons 4 2)]) (car x))))
