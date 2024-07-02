#lang racket/base

(require
 rackunit
 "../../util/check-syntax.rkt")

(test-case
 "Correct capture of variables by a macro"
 (check-not-syntax-error
  (require "../simple-choreographies.rkt" rackunit)
  (define-chor [A]
    (define-local A x 12)

    (define-syntax/chor print-plus-x
      (lambda (stx)
        (syntax-case stx ()
          [(_ P v)
           #'(expr-local P (set! x (+ x v)))])))

    (print-plus-x A 2)
    (expr-local A (check-equal? x 14)))))


(test-case
 "Incorrect capture of variables by a macro"
 (check-has-syntax-error
  (require "../simple-choreographies.rkt")
  (define-syntax print-plus-x
    (chor-macro
     (lambda (stx)
       (syntax-case stx ()
         [(_ P v)
          #'(expr-local P (println (+ x v)))]))))

  (define-chor [A]
    (define-local A x 12)

    (print-plus-x A 2))))
