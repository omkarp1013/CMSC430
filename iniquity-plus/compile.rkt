#lang racket
(provide (all-defined-out))
(require "ast.rkt")
(require "compile-ops.rkt")
(require "types.rkt")
(require a86/ast)

(define rax 'rax)
(define rdx 'rdx) ; scratch (used to hold number of arguments for arity checking)
(define rcx 'rcx) ; scratch
(define r10 'r10)
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg
(define r15 'r15) ; stack pad (non-volatile)

;; Prog -> Asm
(define (compile p)
  (match p
    [(Prog ds e)
     (prog (Global 'entry)
           (Extern 'peek_byte)
           (Extern 'read_byte)
           (Extern 'write_byte)
           (Extern 'raise_error)
           (Label 'entry)
           (Push rbx)    ; save callee-saved register
           (Push r15)
           (Mov rbx rdi) ; recv heap pointer

           (compile-e e '())
           (Pop r15)     ; restore callee-save register
           (Pop rbx)
           (Ret)
           (compile-defines ds)
           (Label 'err)
           pad-stack
           (Call 'raise_error))]))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(Defn f fun)
     (compile-fun f fun)]))

;; Id Fun -> Asm
(define (compile-fun f fun)
  (match fun
    [(FunPlain xs e)
     (seq (Label (symbol->label f))
          ;; TODO: check arity. Done
          (Mov rax (length xs))
          (Cmp rdx rax)
          (Jne 'err)
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs)))
          (Ret))]
    ;; TODO: handle other kinds of functions
    [(FunRest xs x e)
      (let ((start (gensym))
           (end (gensym)))
          
        (seq (Label (symbol->label f))
             (Mov rax (length xs))
             (Cmp rdx rax)
             (Jl 'err)
             (Mov r10 rdx)

             (Mov rcx (value->bits '()))
             
             (Sub r10 rax)

             ;; Main loop to create rest argument list
             (Label start)
             (Cmp r10 0)
             (Je end)
             (Mov (Offset rbx 0) rcx)
             (Pop rcx)
             (Mov (Offset rbx 8) rcx)
             (Mov rcx rbx)
             (Xor rcx type-cons)
             (Sub r10 1)
             (Add rbx 16)
             (Jmp start)
             (Label end)
             
             (Push rcx)
             (compile-e e (cons x (reverse xs)))
             (Add rsp (* 8 (+ (length xs) 1)))
             (Ret)))]
    [(FunCase cs)
      (seq (Label (symbol->label f))
           (fun-case-helper cs)
           (Ret))]))

  
(define (fun-case-helper cs)
    (match cs
      [(cons (FunRest xs x e) l)
        (seq)]
      [(cons (FunPlain xs e) l)
        (let ((end (gensym))
             (next (gensym)))             
          (seq 
               (Mov rax (length xs))
               (Cmp rax rdx)
               (Jne next)
               (Jmp end)

               (Label next)
               (fun-case-helper l) ;; if the amuont of arguments is different
               (Ret)

               (Label end)
               (compile-e e (reverse xs))
               (Add rsp (* 8 (length xs)))
               (Ret)))]

    [_ (Jmp 'err)]))

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
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)]
    [(If e1 e2 e3)
     (compile-if e1 e2 e3 c)]
    [(Begin e1 e2)
     (compile-begin e1 e2 c)]
    [(Let x e1 e2)
     (compile-let x e1 e2 c)]
    [(App f es)
     (compile-app f es c)]
    [(Apply f es e)
     (compile-apply f es e c)]))

;; Value -> Asm
(define (compile-value v)
  (cond [(string? v) (compile-string v)]
        [else        (Mov rax (value->bits v))]))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

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

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))
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
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
	 ;; TODO: communicate argument count to called function. Done.
         (Mov rdx (length es))
         (Jmp (symbol->label f))
         (Label r))))

;; Id [Listof Expr] Expr CEnv -> Asm
(define (compile-apply f es e c)
  ;; TODO: implement apply. Done?
  (let ((start (gensym))
       (end (gensym))
       (return (gensym)))
    (seq  (Lea rax return)
          (Push rax)
          (Mov rdx (length es))

          (compile-es es (cons #f c))
          (compile-e e (append es (cons #f c)))

          (Label start)
          (Cmp rax (value->bits '()))
          (Je end)

          (Xor rax type-cons)
          (Mov r10 (Offset rax 8))
          (Add rdx 1)
          (Push r10)
          (Mov rax (Offset rax 0))
          (Jmp start)

          (Label end)

          (Jmp (symbol->label f))
          (Label return))))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

