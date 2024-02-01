#lang racket
(provide (all-defined-out))
(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; String functions

;; Read up on string functions in Racket to implement these.

;; String String -> String
;; Select the longer of the two strings (or first if same length)
(define (longer s1 s2)
  (if (> (string-length s2) (string-length s1) s2 s1))
)

(module+ test
  (check-equal? (longer "" "") "")
  (check-equal? (longer "abc" "d") "abc")
  (check-equal? (longer "a" "bcd") "bcd")
  (check-equal? (longer "ab" "cd") "ab"))

;; String -> [Listof String]
;; Explode a string into a list of length-1 strings
(define (explode s)
  (define helper i s res
    (match (= i (string-length s))
      [#t res]
      [#f (helper (+ i 1) s (cons (string (string-ref s i)) res))])) 
  (helper 0 s '())'())

(module+ test
  (check-equal? (explode "") '())
  (check-equal? (explode "a") '("a"))
  (check-equal? (explode "abc") '("a" "b" "c")))

;; String -> [Listof [List String String]]
;; Compute list of bigrams (pairs of adjacent letters) in a string
(define (bigrams s)
  (define helper i s res
    (match (<= (string-length s) 2)
      [#t '()]
      [#f 
        (match (string-length i)
          [(- string-length 1) res]
          [(< (- string-length 1)) (helper (+ i 1) (cons res (cons (string-ref i) (string-ref (+ i 1)))))]
        )
      ]
    )
  )

  (helper 0 s '())
)

(module+ test
  (check-equal? (bigrams "") '())
  (check-equal? (bigrams "a") '())
  (check-equal? (bigrams "ab") '(("a" "b")))
  (check-equal? (bigrams "abc") '(("a" "b") ("b" "c"))))
