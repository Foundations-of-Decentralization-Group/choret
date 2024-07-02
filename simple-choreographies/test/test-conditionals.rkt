#lang racket/base

(require
 rackunit
 "../../util/check-syntax.rkt")

(test-case
 "Simple conditional"
 (check-not-syntax-error
  (require "../simple-choreographies.rkt" rackunit)
  (define-chor [A B]
    (define-local A x 1)
    (define-local B y 0)

    (if/chor
     (A (eq? x 1))
     (begin/chor
       (sel-> [A 'OK] [B]
              (com-> [A 1] [B y])))
     (begin/chor
       (sel-> [A 'KO] [B]
              (com-> [A 2] [B y]))))

    (expr-local B (check-equal? y 1)))))


(test-case
 "Nested conditionals"
 (check-not-syntax-error
  (require "../simple-choreographies.rkt" rackunit)
  (define-chor [A B]
    (define-local A x 1)
    (define-local B y 0)
    (define-local A z 0)

    (if/chor
     (A (eq? x 1))
     (sel-> [A 'OK-A] [B]
            (begin/chor
              (com-> [A 1] [B y])
              (if/chor
               (B (eq? y 2))
               (sel-> [B 'OK-B] [A]
                      (com-> [B 1] [A z]))
               (sel-> [B 'KO-B] [A]
                      (com-> [B 2] [A z])))))
     (sel-> [A 'KO-A] [B]
            (begin/chor
              (com-> [A 2] [B y])
              (if/chor
               (B (eq? y 2))
               (sel-> [B 'OK-B] [A]
                      (com-> [B 3] [A z]))
               (sel-> [B 'KO-B] [A]
                      (com-> [B 4] [A z]))))))

    (expr-local A (check-equal? z 2)))))
