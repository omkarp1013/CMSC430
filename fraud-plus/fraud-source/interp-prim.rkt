#lang racket
(provide interp-prim0 interp-prim1 interp-prim2 interp-primN)

;; Op0 -> Answer
(define (interp-prim0 op)
  (match op
    ['read-byte (read-byte)]
    ['peek-byte (peek-byte)]
    ['void      (void)]))

;; Op1 Value -> Answer
(define (interp-prim1 op v)
  (match (list op v)
    [(list 'add1 (? integer?))            (add1 v)]
    [(list 'sub1 (? integer?))            (sub1 v)]
    [(list 'zero? (? integer?))           (zero? v)]
    [(list 'char? v)                      (char? v)]
    [(list 'integer->char (? codepoint?)) (integer->char v)]
    [(list 'char->integer (? char?))      (char->integer v)]
    [(list 'write-byte    (? byte?))      (write-byte v)]
    [(list 'eof-object? v)                (eof-object? v)]
    [(list 'integer? v)                   (integer? v)]
    [(list 'boolean? v)                   (boolean? v)]
    [(list '- v)                          (- v)]
    [(list 'abs v)                        (abs v)]
    ;; TODO: handle -, abs, integer?, etc. DONE
    [_ 'err]))

;; Op2 Value Value -> Answer
(define (interp-prim2 op v1 v2)
  (match (list op v1 v2)
    [(list '- (? integer?) (? integer?)) (- v1 v2)]
    [(list '< (? integer?) (? integer?)) (< v1 v2)]
    [(list '= (? integer?) (? integer?)) (= v1 v2)]
    ;; TODO: Make + take any number of arguments, see hint below. DONE
    ;; Once that works, you can remove this code: 
    [_ 'err]))

;; HINT: You could use a function like the following and call it from interp.

;; OpN [Listof Value] -> Answer
(define (interp-primN op vs res)
  (match (cons (op (check-int-list vs)))
    [(cons '+ #t) 
      (match vs
        ['() res]
        [(cons n xs) (interp-primN op xs (+ n res))])]
     ;; TODO: implement n-ary +. DONE
    [_ 'err]))

(define (check-int-list lst)
  (match lst
    [(cons n xs) (if (integer? n) (check-int-list xs) #f)]
    ['() #t]))

;; Any -> Boolean
(define (codepoint? v)
  (and (integer? v)
       (or (<= 0 v 55295)
           (<= 57344 v 1114111))))

