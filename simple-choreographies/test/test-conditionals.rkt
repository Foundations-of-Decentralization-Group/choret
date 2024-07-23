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


(test-case
 "Code after selction form"
 (check-not-syntax-error
  (require "../simple-choreographies.rkt" rackunit)
  (define-chor [A B]
    (define-local A r 0)
    (define-local A s 0)
    (define-local A t 0)
    (define-local A x 1)
    (define-local B y 0)
    (define-local B z 0)

    (if/chor
     (A (eq? x 1))
     (begin/chor
       (sel-> [A 'OK] [B]
              (begin/chor
                (com-> [A 1] [B y])
                (com-> [B 3] [A r])))
       (com-> [B 5] [A s])
       (com-> [A (+ 10 2)] [B z]))
     (begin/chor
       (sel-> [A 'KO] [B]
              (begin/chor
                (com-> [A 2] [B y])
                (com-> [B 4] [A r])))
       (com-> [B 5] [A t])
       (com-> [A (- 20 4)] [B z])))

    (expr-local A (begin
                    (check-equal? r 3)
                    (check-equal? s 5)
                    (check-equal? t 0)))
    (expr-local B (begin
                    (check-equal? y 1)
                    (check-equal? z 12))))))


(test-case
 "Mismatched number of forms in if/chor form"
 (check-has-syntax-error
  (require "../simple-choreographies.rkt" rackunit)
  (define-chor [A B]
    (define-local A x 0)
    (define-local B y 0)

    (if/chor
     [A (eq? x 1)]
     (begin/chor
       (com-> [A 1] [B y]))
     (begin/chor
       (com-> [A 2] [B y])
       (com-> [A 3] [B y]))))))