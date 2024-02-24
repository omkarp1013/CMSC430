#lang racket

(define (match_cond clauses)
  (cond
    [(empty? clauses) #t] ; Base case: if there are no more clauses, return true
    [(pair? clauses)     ; Recursive case: if there are still clauses left
     (match (first clauses)
       [(list test result)
        (displayln (format "Test: ~v" test))
        (displayln (format "Result: ~v" result))
        (match_cond (rest clauses))])]))

(define s '(asds [(not #t) 1] [(< 2 3) 2] [7 4] [else 3]))

(match_cond (rest s)) ; Start from the second element since the first one is 'cond


(define (all-minus-last lst)
  (reverse (rest (reverse lst)))) 

(define k '(cond [7 4] [else 5]))

(match k 
    [(list 'cond clauses ...) 
        (first clauses)])