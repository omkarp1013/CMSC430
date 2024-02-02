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
  (if (> (string-length s2) (string-length s1)) s2 s1)
)

(module+ test
  (check-equal? (longer "" "") "")
  (check-equal? (longer "abc" "d") "abc")
  (check-equal? (longer "a" "bcd") "bcd")
  (check-equal? (longer "ab" "cd") "ab"))

;; String -> [Listof String]
;; Explode a string into a list of length-1 strings

(define (helper_explode i s res)
  (match (= i (string-length s))
    [#t res]
    [#f (helper_explode (+ i 1) s (append res (list (string (string-ref s i)))))]
  )
)


(define (explode s)
  (helper_explode 0 s '())
)

(module+ test
  (check-equal? (explode "") '())
  (check-equal? (explode "a") '("a"))
  (check-equal? (explode "abc") '("a" "b" "c")))

;; String -> [Listof [List String String]]
;; Compute list of bigrams (pairs of adjacent letters) in a string
(define (helper_bigrams i s res)
  (match (< (string-length s) 2)
    [#t '()]
    [#f 
      (match (= i (- (string-length s) 1))
        [#t res]
        [#f (helper_bigrams (+ i 1) s (append res (list (cons (substring s i (+ i 1)) (cons (substring s (+ i 1) (+ i 2)) '())))))]
      )
    ]
  )
)

(define (bigrams s)
  (helper_bigrams 0 s '())
)

(module+ test
  (check-equal? (bigrams "") '())
  (check-equal? (bigrams "a") '())
  (check-equal? (bigrams "ab") '(("a" "b")))
  (check-equal? (bigrams "abc") '(("a" "b") ("b" "c"))))
