#lang racket
(provide interp-prim0 interp-prim1 interp-prim2)

;; Op0 -> Value
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Value
(define (interp-prim1 op v)
  (match op
    ['add1 (check-bound (add1 v))]
    ['sub1 (check-bound (sub1 v))]
    ['zero? (zero? v)]
    ['char? (char? v)]
    ['integer->char (integer->char v)]
    ['char->integer (char->integer v)]
    ['write-byte    (write-byte v)]
    ['eof-object?   (eof-object? v)]))

;; Op2 Value Value -> Value
(define (interp-prim2 op v1 v2)
  (match op
    ['+ (check-bound (+ v1 v2))]))

;; Value -> Value
(define (check-bound v)
  (if (< (integer-length v) 63)
      v
      (error "undefined behavior")))
