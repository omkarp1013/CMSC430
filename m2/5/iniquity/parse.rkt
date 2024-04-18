#lang racket
(provide parse parse-prog parse-e parse-define correct-arity?)
(require "ast.rkt")

;; S-Expr ... -> Prog
(define (parse . s)  
  (let ((p (apply parse-prog s)))
    (if (correct-arity? p)
        p
        (error "syntactic arity error"))))

;; TODO: implement this function
;; Prog -> Boolean
;; Determine if every application of a function has the correct
;; number of arguments in the program
(define (correct-arity? p)
  (match p
    [(Prog ds e) (correct-arity-exp? ds e)]
    [_ 'err]))

(define (correct-arity-exp? ds e)
  (match e
    [(Lit s) #t]
    [(Var s) #t]
    [(Eof) #t]
    [(Empty) #t]
    [(Prim0 p0) #t]
    [(Prim1 p1 e1) (correct-arity-exp? ds e1)]
    [(Prim2 p2 e1 e2) (and (correct-arity-exp? ds e1) (correct-arity-exp? ds e2))]
    [(Prim3 p3 e1 e2 e3) (and (correct-arity-exp? ds e1) (and (correct-arity-exp? e2) (correct-arity-exp? e3)))]
    [(If e1 e2 e3) (and (correct-arity-exp? ds e1) (and (correct-arity-exp? e2) (correct-arity-exp? e3)))]
    [(Let x e1 e2) (and (correct-arity-exp? ds e1) (correct-arity-exp? ds e2))]
    [(Begin e1 e2) (and (correct-arity-exp? ds e1) (correct-arity-exp? ds e2))]
    [(App f es)
      (match (find-defn f ds)
        [(Defn f1 xs e1)
          (if (= (length es) (length xs)) #t #f)]
        [_ #f])]))

(define (find-defn f ds)
  (match ds
    ['() 'err]
    [(cons (Defn f1 xs e1) dr) (if (equal? f f1) (Defn f1 xs e1) (find-defn f xs))]))

;; S-Expr ... -> Prog
(define (parse-prog . s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (apply parse-prog s)
       [(Prog ds e)
        (Prog (cons (parse-define d) ds) e)])]
    [(cons e '()) (Prog '() (parse-e e))]
    [_ (error "program parse error")]))

;; S-Expr -> Defn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (Defn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? datum?)               (Lit s)]
    ['eof                     (Eof)]
    [(? symbol?)              (Var s)]
    [(list 'quote (list))     (Empty)]
    [(list (? op0? p0))       (Prim0 p0)]
    [(list (? op1? p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? op2? p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? op3? p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error" s)]))


;; Any -> Boolean
(define (datum? x)
  (or (exact-integer? x)
      (boolean? x)
      (char? x)
      (string? x)))

;; Any -> Boolean
(define (op0? x)
  (memq x '(read-byte peek-byte void)))

(define (op1? x)
  (memq x '(add1 sub1 zero? char? integer->char char->integer
                 write-byte eof-object?
                 box unbox empty? cons? box? car cdr
                 vector? vector-length string? string-length)))

(define (op2? x)
  (memq x '(+ - < = eq? cons
              make-vector vector-ref make-string string-ref)))

(define (op3? x)
  (memq x '(vector-set!)))

