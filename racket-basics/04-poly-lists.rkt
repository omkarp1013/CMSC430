#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Polymorphic list functions

;; ∀ (α) (α -> Real) [Pairof α [Listof α]] -> α
;; Find element that minimizes the given measure (take first if more than one)

(define (helper_minimize f xs res)
 (match xs
  ['() (first res)]
  [(cons curr xs)
    (if (< (f curr) (last res)) (helper_minimize f xs (list curr (f curr))) (helper_minimize f xs res))]
  )
)

(define (minimize f xs)
  (helper_minimize f xs (list (first xs) (f (first xs))))
)

(module+ test
  (check-equal? (minimize abs '(1 -2 3)) 1)
  (check-equal? (minimize string-length '("abc" "d" "efg")) "d")
  (check-equal? (minimize string-length '("abc" "d" "ef" "g")) "d"))

;; ∀ (α) (α α -> Boolean) [Listof α] -> [Listof α]
;; Sort list in ascending order according to given comparison
;; ENSURE: result is stable
(define (sort < xs)
  (match xs
    ['() '()]
    [(cons n ls) (insert-desc-1 < n (sort < ls))]
  )
)

(define (insert-desc-1 < n xs)
  (insert-helper-1 < '() n xs)
)

(define (insert-helper-1 < p1 n xs)
  (match xs
    ['() (append p1 (list n))]
    [(cons x res)
     (if (or (and (not (< x n)) (<= (pos n xs) (pos x xs))) (< n x))
         (append p1 (list n) (list x) res)
         (insert-helper-1 < (append p1 (list x)) n res))]
  )
)

(define (pos x xs)
  (if (member x xs)
      (- (length xs) (length (member x xs)))
      -1)
)

(module+ test
  (check-equal? (sort < '(1 -2 3)) '(-2 1 3))
  (check-equal? (sort string<? '("d" "abc" "efg")) '("abc" "d" "efg"))
  (check-equal?
   (sort (λ (s1 s2)
           (< (string-length s1) (string-length s2)))
         '("efg" "d" "abc")) '("d" "efg" "abc")))

;; ∀ (α β) [Listof α] [Listof β] -> [Listof [List α β]]
;; Zip together lists into a list of lists
;; ASSUME: lists are the same length
(define (zip as bs)
  (match (list as bs)
    [(list '() '()) '()]
    [(list (cons a af) (cons b bf))
     (append (list (list a b)) (zip af bf))
    ]
  )
)

(module+ test
  (check-equal? (zip '() '()) '())
  (check-equal? (zip '(1) '(2)) '((1 2)))
  (check-equal? (zip '(1 3) '(2 4)) '((1 2) (3 4)))
  (check-equal? (zip '(1 3) '("a" "b")) '((1 "a") (3 "b"))))

;; ∀ (α) (Listof (α -> α)) -> (α -> α)
;; Compose a list of functions into a single function
;; ((pipe (list f1 f2 f3)) x) ≡ (f1 (f2 (f3 x)))
(define (pipe fs)
  (match fs
    ['() (lambda (x) x)]
    [(cons fi fk)
      (lambda (x) (fi ((pipe fk) x)))]
  )
)

(module+ test
  (check-equal? ((pipe (list number->string sqr add1)) 5) "36")
  (check-equal? ((pipe (list number->string add1 sqr)) 5) "26")
  (check-equal? ((pipe (list string-length number->string add1 sqr)) 5) 2))