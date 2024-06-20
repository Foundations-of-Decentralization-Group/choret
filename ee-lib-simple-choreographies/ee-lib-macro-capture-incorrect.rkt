#lang s-exp "simple-chor-ee-lib.rkt"

(define-syntax print-plus-x
  (chor-macro
   (lambda (stx)
     (syntax-case stx ()
       [(_ P v)
        #'(expr-local P (println (+ x v)))]))))

(define-chor [A]
  (define-local A x 12)

  (print-plus-x A 2))
