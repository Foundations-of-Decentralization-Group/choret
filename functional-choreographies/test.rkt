#lang racket/base

(require
  rackunit
  "../util/check-syntax.rkt")

(test-case
 "Simple communication"
 (check-not-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2)
        (define (at l1 x) 0)
        (define (at l2 x) 0)
        (at l1 0)
        (~> (at l1 10) (at l2 x))
        (at l2 (println x))
        (at l1 (println x)))))

(test-case
 "Simple conditional without selection"
 (check-not-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2)
        (define (at l2 x) 0)
        (if (at l1 #f)
            (~> (at l1 10) (at l2 x))
            (~> (at l1 15) (at l2 x)))
        (at l2 (println x)))))

(test-case
 "Simple conditional with selection"
 (check-not-syntax-error
  (require rackunit "functional-choreographies.rkt")
  (chor (l1 l2)
        (define (at l2 x) 0)
        (define (at l2 y) 0)
        (if (at l1 #f)
            (sel~> l1 [l2 'x (~> (at l1 10) (at l2 x))])
            (sel~> l1 [l2 'y (~> (at l1 10) (at l2 y))]))
        (at l2 (check-equal? x 0))
        (at l2 (check-equal? y 10)))))

(test-case
 "Nested selections"
 (check-not-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2)
        (define (at l2 x) 0)
        (if (at l1 #t)
            (sel~> l1
                   [l2 'test1
                       (begin (at l2 (println 1))
                              (if (at l1 #t)
                                  (sel~> l1
                                         [l2 'test3
                                             (at l2 (println 3))])
                                  (sel~> l1
                                         [l2 'test4
                                             (at l2 (println 4))])))])
            (sel~> l1
                   [l2 'test2
                       (begin (at l2 (println 2)))]))
        (at l2 (println x)))))

(test-case
 "Nested merges"
 (check-not-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2)
        (define (at l2 x) 0)
        (define (at l2 res)
          (if (at l1 #t)
              (if (at l1 #f)
                  (sel~> l1 [l2 'test1 (at l2 1)])
                  (sel~> l1 [l2 'test2 (at l2 2)]))
              (if (at l1 #t)
                  (sel~> l1 [l2 'test2 (at l2 2)])
                  (sel~> l1 [l2 'test3 (at l2 3)]))))
        (at l2 (println res)))))

(test-case
 "let with multiple bindings"
 (check-not-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2 l3)
        (define (at l3 x) 0)
        (define (at l3 y) 0)
        (let ([(at l1 x) 10]
              [(at l2 y) 15])
          (~> (at l1 x) (at l3 x))
          (~> (at l2 y) (at l3 y)))
        (at l3 (println (+ x y))))))

(test-case
 "Nested let forms"
 (check-not-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2 l3)
        (define (at l3 x) 0)
        (define (at l3 y) 0)
        (let ([(at l1 x) 10])
          (~> (at l1 x) (at l3 x))
          (let ([(at l2 y) 15])
            (~> (at l2 y) (at l3 y))))
        (at l3 (println (+ x y))))))

(test-case
 "Reference let binding out of scope"
 (check-has-syntax-error
  (require "functional-choreographies.rkt")
  (chor (l1 l2 l3)
        (define (at l3 x) 0)
        (define (at l3 y) 0)
        (let ([(at l1 x) 10])
          (~> (at l1 x) (at l3 x))
          (let ([(at l2 y) 15])
            (~> (at l2 y) (at l3 y))))
        (at l1 (println x))
        (at l3 (println (+ x y))))))
