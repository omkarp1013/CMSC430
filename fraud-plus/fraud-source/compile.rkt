#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define rdx 'rdx)
(define rsp 'rsp) ; stack
(define r15 'r15) ; stack pad (non-volatile)

;; Expr -> Asm
(define (compile e)
  (prog (Global 'entry)
        (Extern 'peek_byte)
        (Extern 'read_byte)
        (Extern 'write_byte)
        (Extern 'raise_error)
        (Label 'entry)
        ;; save callee-saved register
        (Push r15)
        (compile-e e '())
        ;; restore callee-save register
        (Pop r15)
        (Ret)
        ;; Error handler
        (Label 'err)
        pad-stack
        (Call 'raise_error)))

;; type CEnv = (Listof [Maybe Id])
;; Expr CEnv -> Asm
(define (compile-e e c)
  (match e
    [(Lit d) (compile-value d)]
    [(Eof) (compile-value eof)]
    [(Var x) (compile-variable x c)]
    [(Prim0 p) (compile-prim0 p)]
    [(Prim1 p e) (compile-prim1 p e c)]
    [(Prim2 p e1 e2) (compile-prim2 p e1 e2 c)]
    ;; TODO: implement n-ary primitives
    [(PrimN p es)    (compile-primN p es c)]    
    [(If e1 e2 e3)
     (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)
     (compile-begin e1 e2 c)]
    ;; TODO: this only works for single variable binding,
    ;; make it work in general
    [(Let cs es e) (seq
                    (compile-e* es c)
                    (compile-let cs es e c))]
    ;; TODO: implement let*, case, cond
    [(Let* cs es e)  (compile-let* cs es e c)]
    [(Case ev cs el) (compile-case ev cs el c)]
    [(Cond cs el)    (compile-cond cs el c)]))


;; [ListofId] [Listof Expr] Expr CEnv -> Asm
(define (compile-let* cs es e c)
  (match cs
    ['() 
      (seq
          (compile-e e c))]
    [(cons n xs)
      (seq
        (compile-e (car es) c)
        (Push rax)
        (compile-let* (cdr cs) (cdr es) e (cons n c))
        (Add rsp 8))]))

;; [ListOfId] [Listof Expr] Expr CEnv -> Asm
(define (compile-let cs es e c)
  (match cs
    ['() 
      (seq 
          (compile-e e c))]
    [(cons n xs) 
      (seq
        (compile-let xs es e (cons n c))
        (Add rsp 8))]))

;; OpN OpN [ListOf Expr] -> Asm
(define (compile-primN p es c)
  (match p
    ['+ (match es
          ['() (seq 
                (Mov rax 0))]
          [(cons n xs)
                (seq
                  (compile-e n c)
                  (Push rax)
                  (compile-primN '+ xs (cons #f c))
                  (compile-op2 p))])]
    [_ (Jmp 'err)]))

;; Expr [Listof CaseClause] Expr CEnv -> Asm
(define (compile-case ev cs el c)
  (let ((end (gensym 'case)))
    (seq 
      (compile-e ev c)
      (compile-case-lst cs end c)
      (compile-e el c)
      (Label end))))  

;; [ListofCaseClause] Expr CEnv -> Asm
(define (compile-case-lst cs end c)
  (match cs
    ['() (seq)]
    [(cons (Clause p b) xs)
      (let ((equal (gensym 'lst))
           (clause (gensym 'lst)))
        (seq (compile-case-clause (Clause p b) end clause equal)
            (Label equal)
            (compile-e b c)
            (Jmp end)
            (Label clause)
            (compile-case-lst xs end c)))]))

;; CaseClause Label Label Label -> Asm
(define (compile-case-clause clause-exp end clause equal)
  (match clause-exp
    [(Clause p b)
      (match p
        ['() (seq (Jmp clause))]
        [(cons n xs)
          (seq (Mov rdx (value->bits n))
               (Cmp rax rdx)
               (Je equal)
               (compile-case-clause (Clause xs b) end clause equal))])]))

;; [Listof CondClause] Expr -> Asm
(define (compile-cond cs e c)
  (match cs
    ['() (compile-e e c)]  
    [(cons (Clause p b) xs)
      (let ((l1 (gensym 'cond))
            (l2 (gensym 'cond)))
        (seq (compile-e p c)
             (Cmp rax (value->bits #f))
             (Je l1)
             (compile-e b c)
             (Jmp l2)
             (Label l1)
             (compile-cond xs e c)
             (Label l2)))]))

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (value->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; Op0 -> Asm
(define (compile-prim0 p)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))


;; HINT: Another potentially helpful function that
;; emits code to execute each expression and push
;; all the values on to the stack, analogous to interp*-env

;; [Listof Expr] CEnv -> Asm
(define (compile-e* es c)
  (match es
    ['() (seq)]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-e* es (cons #f c)))]))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax (value->bits #f))
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))
;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
;; NOTE: this is specialized for a single variable binding
;; You should write another function for the general case
(define (compile-let1 x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

