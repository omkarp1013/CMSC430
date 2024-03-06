#lang racket
(require "../compile.rkt"
         "../interp.rkt"
         "../interp-io.rkt"
         "../parse.rkt"
         "../types.rkt"
	 "../build-runtime.rkt"
         a86/interp
         rackunit)


;; link with runtime for IO operations
;(unless (file-exists? "../runtime.o")
;  (system "make -C .. runtime.o"))
(current-objs
 (list (path->string runtime-path)))

(define (test-runner-io run)
  ;; Repeat tests
  (run '(repeat 1 (write-byte 97)) "")
  (run '(repeat 5 (write-byte 97)) "")
  (run '(repeat 5 (write-byte (peek-byte))) "a")
  (run '(repeat 0 (write-byte 97)) "")
  (run '(repeat (repeat 0 (write-byte 97)) (write-byte 97)) "")
  (run '(repeat 5 (repeat 2 (write-byte 97))) "")
  (run '(repeat 5 (repeat 4 (repeat 3 (write-byte 97)))) ""))
  ;;; (check-equal? (run 7 "") (cons 7 ""))
  ;;; (check-equal? (run '(write-byte 97) "") (cons (void) "a"))
  ;;; (check-equal? (run '(read-byte) "a") (cons 97 ""))
  ;;; (check-equal? (run '(begin (write-byte 97) (read-byte)) "b")
  ;;;               (cons 98 "a"))
  ;;; (check-equal? (run '(read-byte) "") (cons eof ""))
  ;;; (check-equal? (run '(eof-object? (read-byte)) "") (cons #t ""))
  ;;; (check-equal? (run '(eof-object? (read-byte)) "a") (cons #f ""))
  ;;; (check-equal? (run '(begin (write-byte 97) (write-byte 98)) "")
  ;;;               (cons (void) "ab"))

  ;;; (check-equal? (run '(peek-byte) "ab") (cons 97 ""))
  ;;; (check-equal? (run '(begin (peek-byte) (read-byte)) "ab") (cons 97 ""))  
  ;;; ;; Extort examples
  ;;; (check-equal? (run '(write-byte #t) "") (cons 'err ""))

;; (test-runner-io (λ (e s) (interp/io (parse e) s)))
(test-runner-io (λ (e s)
                  (match (asm-interp/io (compile (parse e)) s)
                    [(cons 'err o) (cons 'err o)]
                    [(cons r o)
                     (cons (bits->value r) o)])))
