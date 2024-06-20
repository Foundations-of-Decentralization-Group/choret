#lang s-exp "simple-chor-ee-lib.rkt"

(require racket/base (for-syntax racket/base))

(define-syntax print-plus-x
  (choret-macro
   (lambda (stx)
     (syntax-case stx ()
       [(_ P v)
        #'(local-expr P (println (+ x v)))]))))

(define-chor [A]
  (local-define A x 12)

  (print-plus-x A 2))
