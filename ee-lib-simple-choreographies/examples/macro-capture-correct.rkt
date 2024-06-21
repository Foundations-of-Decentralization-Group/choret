#lang s-exp "../simple-choreographies.rkt"

(define-chor [A]
  (define-local A x 12)

  (define-syntax/chor print-plus-x
    (lambda (stx)
      (syntax-case stx ()
        [(_ P v)
         #'(expr-local P (println (+ x v)))])))

  (print-plus-x A 2))
