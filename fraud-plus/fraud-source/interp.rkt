#lang racket
(provide interp)
(provide interp-env)
(require "ast.rkt")
(require "interp-prim.rkt")

;; type Answer = Value | 'err

;; type Value =
;; | Integer
;; | Boolean
;; | Character
;; | Eof
;; | Void

;; type Env = (Listof (List Id Value))

;; Expr -> Answer
(define (interp e)
  (interp-env e '()))

;; Expr Env -> Answer
(define (interp-env e r)
  (match e
    [(Lit d) d]
    [(Eof)   eof]
    [(Var x) (lookup r x)]
    [(Prim0 p) (interp-prim0 p)]
    [(Prim1 p e)
     (match (interp-env e r)
       ['err 'err]
       [v (interp-prim1 p v)])]
    [(Prim2 p e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v1 (match (interp-env e2 r)
             ['err 'err]
             [v2 (interp-prim2 p v1 v2)])])]
    ;; TODO: implement n-ary primitive. DONE
    [(PrimN p es) (interp-primN p es 0)]
    [(If e0 e1 e2)
     (match (interp-env e0 r)
       ['err 'err]
       [v
        (if v
            (interp-env e1 r)
            (interp-env e2 r))])]
    [(Begin e1 e2)
     (match (interp-env e1 r)
       ['err 'err]
       [v    (interp-env e2 r)])]
    ;; TODO: implement cond. DONE
    [(Cond cs e) (interp-cond cs e r)]
    ;; TODO: implement case. DONE
    [(Case ev cs el) (interp-case ev cs el)]
    ;; TODO: this works for just a single binding
    ;; but you need to make it work in general
    [(Let )
     ()
     (match (interp-env e1 r)
       ['err 'err]
       [v (interp-env e2 (ext r x v))])]
    ;; TODO: implement let*
    [(Let* xs es e) 'err]))

(define (interp-cond cs e r)
  (match cs
    ['() (interp-env e r)]
    [(cons (Clause p b) xs)
      (if (interp*-clause cs r) (interp-cond-clauses cs e r) 'err)]))

(define (interp*-clause cs r)
  (match cs
    ['() #t]
    [(cons (Clause p b) xs)
      (match (cons (interp-env p r) (interp-env b r))
        [(cons 'err _) #f]
        [(cons _ 'err) #f]
        [_ (interp*-clause xs r)])]))

(define (interp-cond-clauses cs e r)
  (match cs
    ['() (interp e)]
    [(cons (Clause p b) xs)
      (match (interp-env p r)
        [#f (interp-cond-clauses xs e r)]
        [_ (interp-env b r)])]))


(define (interp-case ev cs el r)
  (match cs
    ['() (interp-env el r)]
    [(cons (Clause p b) xs)
      (match (interp-env ev r)
        ['err 'err]
        [_ (interp-case-clauses (interp-env ev r) cs el r)])]))

(define (interp-case-clauses v cs el r)
  (match cs
    ['() (interp-env el r)]
    [(cons (Clause p b) xs)
      (if (member v p) (interp b) (interp-case-clauses v xs el r))]))

;; HINT: this is a function that may come in handy.
;; It takes a list of expressions and environment
;; and evaluates each expression in order.  If any
;; expression produces 'err, the whole thing produces
;; 'err; otherwise it produces a list of values.

;; type Answer* = 'err | [Listof Value]
;; [Listof Expr] Env -> Answer*
(define (interp*-env es r)
  (match es
    ['() '()]
    [(cons e es)
     (match (interp-env e r)
       ['err 'err]
       [v (match (interp*-env es r)
            ['err 'err]
            [vs (cons v vs)])])]))

;; Env Id -> Value
(define (lookup r x)
  (match r
    [(cons (list y val) r)
     (if (symbol=? x y)
         val
         (lookup r x))]))


;; Env Id Value -> Env
(define (ext r x v)
  (cons (list x v) r))

