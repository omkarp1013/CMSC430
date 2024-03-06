 #lang racket
(provide Lit Prim0 Prim1 Prim2 If Eof Begin Let
         Var)
;;
;; type Expr = (Lit Datum)
;;           | (Eof)
;;           | (Prim0 Op0)
;;           | (Prim1 Op1 Expr)
;;           | (Prim2 Op2 Expr Expr)
;;           | (If Expr Expr Expr)
;;           | (Let Id Expr Expr)
;;           | (Var Id)

;; type Id  = Symbol
;; type Datum = Integer
;;            | Boolean
;;            | Character
;; type Op0 = 'read-byte | 'peek-byte | 'void
;; type Op1 = 'add1 | 'sub1
;;          | 'zero?
;;          | 'char? | 'integer->char | 'char->integer
;;          | 'write-byte | 'eof-object?
;; type Op2 = '+ | '- | '< | '=

(struct Eof () #:prefab)
(struct Lit (d) #:prefab)
(struct Prim0 (p) #:prefab)
(struct Prim1 (p e) #:prefab)
(struct Prim2 (p e1 e2)  #:prefab)
(struct If (e1 e2 e3) #:prefab)
(struct Begin (e1 e2) #:prefab)
(struct Let (x e1 e2) #:prefab)
(struct Var (x) #:prefab)


;; Expr -> Natural
;; Compute the length of the largest environment
;; that will be used when interpreting this expression.
;; ASSUME: the expression is closed (has no free variables)

(define (max-env-length e)
  (match e
    [(Eof) 0]
    [(Lit d) 0]
    [(Prim0 p) 0]
    [(Prim1 p e) (max-env-length e)]
    [(Prim2 p e1 e2) (max (max-env-length e1) (max-env-length e2))]
    [(Begin e1 e2) (max (max-env-length e1) (max-env-length e2))]
    [(If e1 e2 e3) (max (max-env-length e1) (max-env-length e2) (max-env-length e3))]
    [(Let x e1 e2) (+ 1 (max (max-env-length e1) (max-env-length e2)))]
    [(Var x) 0]))


(module+ test
  (require rackunit)
  (check-equal? (max-env-length (Lit 5)) 0)
  (check-equal? (max-env-length (Let 'x (Lit 5) (Var 'x))) 1)
  (check-equal? (max-env-length (Prim2 '+
                                       (Let 'x (Lit 5) (Var 'x))
                                       (Let 'y (Lit 6) (Var 'y))))
                1)
  (check-equal? (max-env-length (Prim2 '+
                                      (Let 'x (Let 'z (Lit 100) (Let 'y (Var 'z) (Let 'k (Var 'y) (Var 'k)))) (Let 'y (Var 'x) (Var 'y))) (Lit 0))) 4)
  (check-equal? (max-env-length (Prim2 '+
                                    (Let 'x (Let 'z (Lit 100) (Var 'z)) (Let 'y (Var 'x) (Var 'y))) (Lit 0))) 2)
  (check-equal? (max-env-length (Let 'x (Lit 100) (Let 'y (Let 'z (Lit 100) (Var 'z)) (Var 'y)))) 3)
  (check-equal? (max-env-length (Eof)) 0)
  (check-equal? (max-env-length (Prim0 'read-byte)) 0)
  (check-equal? (max-env-length (Prim1 'add1 (Let 'x (Lit 100) (Var 'x)))) 1)
  (check-equal? (max-env-length (Begin (Let 'x (Lit 100) (Var 'x)) (Lit 100))) 1)
  (check-equal? (max-env-length (Var 'x)) 0))