#lang racket
(provide (all-defined-out))

;; Integer -> Value
(define (bits->value b)
  (cond [(= b (value->bits #t)) #t]
        [(= b (value->bits #f)) #f]
        [(negative? b) b]
        [else (- b 2)]))

  ;; Value -> Integer
(define (value->bits v)
  (cond [(eq? v #t) 1]
        [(eq? v #f) 0]
        [(negative? v) v]
        [else (+ v 2)]))

(define (int-bits? v)
  (and (not (= v 0))
       (not (= v 1))))

