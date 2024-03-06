#lang racket
(provide F F-concrete F-pre 𝑭 𝑭-𝒆𝒏𝒗 lookup ext)
(require redex/reduction-semantics
         "../extort/semantics.rkt")

; for use in presentations (informally noting x can't be let, etc.)
(define-extended-language F-pre E-concrete
  (e ::= .... x (let ((x e)) e) (p e))
  (p ::= add1 sub1 zero?)
  (x ::= variable))

;; the real grammar language
(define-extended-language F-concrete F-pre
  (x ::= variable-not-otherwise-mentioned)
  (r ::= ((x v) ...)))

(define-extended-language F E
  (x ::= variable)
  (r ::= ((x v) ...))
  (e ::= .... (Var x) (Let x e e)))

(module+ test
  (test-equal (redex-match? F-concrete e (term x)) #t)
  (test-equal (redex-match? F-concrete e (term let)) #f)
  (test-equal (redex-match? F-concrete e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? F-concrete e (term (let ((let 1)) 3))) #f))

(module+ test
  (test-equal (redex-match? F-pre e (term x)) #t)
  (test-equal (redex-match? F-pre e (term let)) #t)
  (test-equal (redex-match? F-pre e (term (let ((x 1)) x))) #t)
  (test-equal (redex-match? F-pre e (term (let ((let 1)) 3))) #t))

(module+ test
  (test-equal (redex-match? F e (term (Var x))) #t)
  (test-equal (redex-match? F e (term (Var let))) #t)
  (test-equal (redex-match? F e (term (Let x (Int 1) (Var x)))) #t)
  (test-equal (redex-match? F e (term (Let let (Int 1) (Int 3)))) #t))

(define-judgment-form F
  #:contract (𝑭 e a)
  #:mode (𝑭 I O)
  [(𝑭-𝒆𝒏𝒗 e () a)
   ---------- "mt-env"
   (𝑭 e a)])

(define-judgment-form F
  #:contract (𝑭-𝒆𝒏𝒗 e r a)
  #:mode (𝑭-𝒆𝒏𝒗 I I O)

  ;; Value
  [----------- "int-lit"
   (𝑭-𝒆𝒏𝒗 (Int i) r i)]
  [----------- "bool-lit"
   (𝑭-𝒆𝒏𝒗 (Bool b) r b)]  

  ;; If
  [(𝑭-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-true v_0)) (𝑭-𝒆𝒏𝒗 e_1 r a)
   -------- "if-true"
   (𝑭-𝒆𝒏𝒗 (If e_0 e_1 e_2) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-false v_0)) (𝑭-𝒆𝒏𝒗 e_2 r a)
   -------- "if-false"
   (𝑭-𝒆𝒏𝒗 (If e_0 e_1 e_2) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r err)
   -------- "if-err"
   (𝑭-𝒆𝒏𝒗 (If e_0 e_1 e_2) r err)]

  ;; Let and variable
  [(where a (lookup r x))
   ----------- "var"
   (𝑭-𝒆𝒏𝒗 (Var x) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r v_0) (𝑭-𝒆𝒏𝒗 e_1 (ext r x v_0) a)
   ----- "let"
   (𝑭-𝒆𝒏𝒗 (Let x e_0 e_1) r a)]

  [(𝑭-𝒆𝒏𝒗 e_0 r err)
   ----------- "let-err"
   (𝑭-𝒆𝒏𝒗 (Let x e_0 e_1) r err)]

  ;; Primitive application
  [(𝑭-𝒆𝒏𝒗 e_0 r a_0)
   ----------- "prim"
   (𝑭-𝒆𝒏𝒗 (Prim1 p1 e_0) r (𝑭-𝒑𝒓𝒊𝒎 p1 a_0))])

(module+ test
  (test-judgment-holds (𝑭 (Int 7) 7))
  (test-judgment-holds (𝑭 (Prim1 'add1 (Int 7)) 8))

  (test-judgment-holds (𝑭 (Prim1 'add1 (Bool #f)) err))
  
  (test-judgment-holds (𝑭 (Let x (Int 7) (Int 8)) 8))
  (test-judgment-holds (𝑭 (Let x (Int 7) (Var x)) 7))
  (test-judgment-holds (𝑭 (Let x (Int 7) (Prim1 'add1 (Var x))) 8))
  (test-judgment-holds (𝑭 (Prim1 'sub1 (Let x (Int 7) (Prim1 'add1 (Var x)))) 7))
  (test-judgment-holds (𝑭 (Prim1 'sub1 (Let x (Int 7)
                                          (Let y (Var x)
                                               (Prim1 'add1 (Var x)))))
                          7))
  (test-judgment-holds (𝑭 (Prim1 'sub1 (Let x (Int 7)
                                          (Let x (Int 8)
                                               (Prim1 'add1 (Var x)))))
                          8)))

;; replace any free variables with 0
(define-metafunction F
  F-close-with-zero : e (x ...) -> e
  [(F-close-with-zero (Var x) (x_0 ... x x_1 ...)) (Var x)]
  [(F-close-with-zero (Var x) any) (Int 0)]
  [(F-close-with-zero (Int i) any) (Int i)]
  [(F-close-with-zero (Bool b) any) (Bool b)]
  [(F-close-with-zero (If e_1 e_2 e_3) any_r)
   (If (F-close-with-zero e_1 any_r)
       (F-close-with-zero e_2 any_r)
       (F-close-with-zero e_3 any_r))]
  [(F-close-with-zero (Prim1 p1 e_1) any_r)
   (Prim1 p1 (close-with-zero e_1 any_r))]
  #;[(F-close-with-zero (Prim2 p2 e_1 e_2) any_r)
   (Prim2 p2
          (close-with-zero e_1 any_r)
          (close-with-zero e_2 any_r))]
  [(F-close-with-zero (Let x e_1 e_2) (x_0 ...))
   (Let x (close-with-zero e_1 (x_0 ...))
        (close-with-zero e_2 (x x_0 ...)))])


(module+ test
  (require rackunit)
  ;; Check that the semantics is total function on closed expressions
  (redex-check F e
               (redex-let F ([e_0 (term (F-close-with-zero e ()))])
                          (check-true (redex-match? F (a_0) (judgment-holds (𝑭 e_0 a) a)) (format "~a" (term e))))
               #:print? #f))



;;;;;;;


(provide G G-concrete 𝑮 𝑮-𝒆𝒏𝒗 𝑭-𝒑𝒓𝒊𝒎)

(define-extended-language G-concrete F-concrete
  (e ::= x i b (if e e e) (let ((x e)) e) (p1 e) (p2 e e))
  (p2 ::= + - < =)
  (p1 ::= add1 sub1 zero?)
  (p ::= p1 p2))

(define-extended-language G F
  (e ::= .... (Prim2 p2 e e))
  (p2 ::= '+ '- '< '=)
  (p ::= p1 p2))
  
(define-judgment-form G
  #:contract (𝑮 e a)
  #:mode (𝑮 I O)
  [(𝑮-𝒆𝒏𝒗 e () a)
   ----------
   (𝑮 e a)])

(define-judgment-form G
  #:contract (𝑮-𝒆𝒏𝒗 e r a)
  #:mode (𝑮-𝒆𝒏𝒗 I I O)

  ;; Value
  [----------- "int-lit"
   (𝑮-𝒆𝒏𝒗 (Int i) r i)]
  [----------- "bool-lit"
   (𝑮-𝒆𝒏𝒗 (Bool b) r b)]  

  ;; If
  [(𝑮-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-true v_0)) (𝑮-𝒆𝒏𝒗 e_1 r a)
   -------- "if-true"
   (𝑮-𝒆𝒏𝒗 (If e_0 e_1 e_2) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r v_0) (side-condition (is-false v_0)) (𝑮-𝒆𝒏𝒗 e_2 r a)
   -------- "if-false"
   (𝑮-𝒆𝒏𝒗 (If e_0 e_1 e_2) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r err)
   -------- "if-err"
   (𝑮-𝒆𝒏𝒗 (If e_0 e_1 e_2) r err)]

  ;; Let and variable
  [(where a (lookup r x))
   ----------- "var"
   (𝑮-𝒆𝒏𝒗 (Var x) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r v_0) (𝑮-𝒆𝒏𝒗 e_1 (ext r x v_0) a)
   ----- "let"
   (𝑮-𝒆𝒏𝒗 (Let x e_0 e_1) r a)]

  [(𝑮-𝒆𝒏𝒗 e_0 r err)
   ----------- "let-err"
   (𝑮-𝒆𝒏𝒗 (Let x e_0 e_1) r err)]

  ;; Primitive application
  [(𝑮-𝒆𝒏𝒗 e_0 r a_0)
   ----------- "prim1"
   (𝑮-𝒆𝒏𝒗 (Prim1 p1 e_0) r (𝑭-𝒑𝒓𝒊𝒎 p1 a_0))]

  [(𝑮-𝒆𝒏𝒗 e_0 r a_0)
   (𝑮-𝒆𝒏𝒗 e_1 r a_1)
   ----------- "prim2"
   (𝑮-𝒆𝒏𝒗 (Prim2 p2 e_0 e_1) r (𝑭-𝒑𝒓𝒊𝒎 p2 a_0 a_1))])

(define-metafunction G
  𝑭-𝒑𝒓𝒊𝒎 : p a ... -> a
  [(𝑭-𝒑𝒓𝒊𝒎 p v ... err _ ...) err]
  [(𝑭-𝒑𝒓𝒊𝒎 'add1 i_0) ,(+ (term i_0) (term 1))]
  [(𝑭-𝒑𝒓𝒊𝒎 'sub1 i_0) ,(- (term i_0) (term 1))]
  [(𝑭-𝒑𝒓𝒊𝒎 'zero? 0) #t]
  [(𝑭-𝒑𝒓𝒊𝒎 'zero? i) #f]
  [(𝑭-𝒑𝒓𝒊𝒎 '+ i_0 i_1) ,(+ (term i_0) (term i_1))]
  [(𝑭-𝒑𝒓𝒊𝒎 '- i_0 i_1) ,(- (term i_0) (term i_1))]
  [(𝑭-𝒑𝒓𝒊𝒎 '< i_0 i_1) ,(< (term i_0) (term i_1))]
  [(𝑭-𝒑𝒓𝒊𝒎 '= i_0 i_1) ,(< (term i_0) (term i_1))]    
  [(𝑭-𝒑𝒓𝒊𝒎 _ ...) err])

(define-metafunction G
  ext : r x v -> r
  [(ext ((x_0 v_0) ...) x v)
   ((x v) (x_0 v_0) ...)])

(define-metafunction G
  lookup : r x -> a
  [(lookup ((x v) (x_1 v_1) ...) x) v]
  [(lookup ((x_0 v_0) (x_1 v_1) ...) x)
   (lookup ((x_1 v_1) ...) x)])

(define-metafunction G
  is-true : v -> boolean
  [(is-true #f) #f]
  [(is-true v)  #t])

(define-metafunction G
  is-false : v -> boolean
  [(is-false #f) #t]
  [(is-false v)  #f])

(module+ test
  (test-judgment-holds (𝑮 (Int 7) 7))
  (test-judgment-holds (𝑮 (Prim1 'add1 (Int 7)) 8))

  (test-judgment-holds (𝑮 (Prim1 'add1 (Bool #f)) err))
  
  (test-judgment-holds (𝑮 (Let x (Int 7) (Int 8)) 8))
  (test-judgment-holds (𝑮 (Let x (Int 7) (Var x)) 7))
  (test-judgment-holds (𝑮 (Let x (Int 7) (Prim1 'add1 (Var x))) 8))
  (test-judgment-holds (𝑮 (Prim1 'sub1 (Let x (Int 7) (Prim1 'add1 (Var x)))) 7))
  (test-judgment-holds (𝑮 (Prim1 'sub1 (Let x (Int 7)
                                           (Let y (Var x)
                                                (Prim1 'add1 (Var x)))))
                          7))
  (test-judgment-holds (𝑮 (Prim1 'sub1 (Let x (Int 7)
                                           (Let x (Int 8)
                                                (Prim1 'add1 (Var x)))))
                          8))

  (test-judgment-holds (𝑮 (Prim2 '+ (Int 1) (Int 2)) 3))
  (test-judgment-holds (𝑮 (Prim2 '- (Int 1) (Int 2)) -1))
  (test-judgment-holds (𝑮 (Prim1 'add1 (Bool #f)) err))
  (test-judgment-holds (𝑮 (If (Prim1 'add1 (Bool #f)) (Int 1) (Int 2)) err))
  (test-judgment-holds (𝑮 (Prim2 '+ (Int 1) (Prim1 'add1 (Bool #f))) err))
  (test-judgment-holds (𝑮 (Prim2 '+ (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑮 (Prim2 '- (Int 1) (Bool #f)) err))
  (test-judgment-holds (𝑮 (Prim2 '- (Prim1 'add1 (Bool #f)) (Bool #f)) err)))

;; replace any free variables with 0
(define-metafunction G
  close-with-zero : e (x ...) -> e
  [(close-with-zero (Var x) (x_0 ... x x_1 ...)) (Var x)]
  [(close-with-zero (Var x) any) (Int 0)]
  [(close-with-zero (Int i) any) (Int i)]
  [(close-with-zero (Bool b) any) (Bool b)]
  [(close-with-zero (If e_1 e_2 e_3) any_r)
   (If (close-with-zero e_1 any_r)
       (close-with-zero e_2 any_r)
       (close-with-zero e_3 any_r))]
  [(close-with-zero (Prim1 p1 e_1) any_r)
   (Prim1 p1 (close-with-zero e_1 any_r))]
  [(close-with-zero (Prim2 p2 e_1 e_2) any_r)
   (Prim2 p2
          (close-with-zero e_1 any_r)
          (close-with-zero e_2 any_r))]
  [(close-with-zero (Let x e_1 e_2) (x_0 ...))
   (Let x (close-with-zero e_1 (x_0 ...))
        (close-with-zero e_2 (x x_0 ...)))])

(module+ test
  (require rackunit)
  ;; Check that the semantics is total function -- for closed expressions
  (redex-check G e
               (redex-let G ([e_0 (term (close-with-zero e ()))])
                          (check-true (redex-match? G (a_0) (judgment-holds (𝑮 e_0 a) a))))
               #:print? #f))
