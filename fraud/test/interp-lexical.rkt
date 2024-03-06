#lang racket
(require rackunit)
(require "../interp-lexical.rkt" "../parse.rkt")
(define (run p)
  (interp (parse p)))
(check-equal? (run 5) 5)
(check-equal? (run '(let ((x 0)) x)) 0)
(check-equal? (run '(let ((x 0)) (let ((y 1)) x))) 0)
(check-equal? (run '(let ((x 0)) (let ((y 1)) y))) 1)
(check-equal? (run '(let ((x 0)) (let ((y x)) y))) 0)

(parse '(+ (let ((x (let ((z 100)) (let ((y z)) y)))) (let ((y x)) y)) 0))
