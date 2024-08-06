#lang racket/base

(require
  rackunit
  "../util/check-syntax.rkt")

(test-case
 "Simple communication"
 (check-not-syntax-error
  (require choret)
  (chor (l1 l2)
        (define (at l1 x) 0)
        (define (at l2 x) 0)
        (at l1 0)
        (set! (at l2 x) (~> (at l1 10) l2))
        (at l2 (println x))
        (at l1 (println x)))))

(test-case
 "Simple conditional without selection"
 (check-not-syntax-error
  (require choret)
  (chor (l1 l2)
        (define (at l2 x) 0)
        (if (at l1 #f)
            (set! (at l2 x) (~> (at l1 10) l2))
            (set! (at l2 x) (~> (at l1 15) l2)))
        (at l2 (println x)))))

(test-case
 "Simple conditional with selection"
 (check-not-syntax-error
  (require rackunit choret)
  (chor (l1 l2)
        (define (at l2 x) 0)
        (define (at l2 y) 0)
        (if (at l1 #f)
            (sel~> l1 [l2 'x (set! (at l2 x) (~> (at l1 10) l2))])
            (sel~> l1 [l2 'y (set! (at l2 y) (~> (at l1 10) l2))]))
        (at l2 (check-equal? x 0))
        (at l2 (check-equal? y 10)))))

(test-case
 "Nested selections"
 (check-not-syntax-error
  (require choret)
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
  (require choret)
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
  (require choret)
  (chor (l1 l2 l3)
        (define (at l3 x) 0)
        (define (at l3 y) 0)
        (let ([(at l1 x) 10]
              [(at l2 y) 15])
          (set! (at l3 x) (~> (at l1 x) l3))
          (set! (at l3 y) (~> (at l2 y) l3)))
        (at l3 (println (+ x y))))))

(test-case
 "Nested let forms"
 (check-not-syntax-error
  (require choret)
  (chor (l1 l2 l3)
        (define (at l3 x) 0)
        (define (at l3 y) 0)
        (let ([(at l1 x) 10])
          (set! (at l3 x) (~> (at l1 x) l3))
          (let ([(at l2 y) 15])
            (set! (at l3 y) (~> (at l2 y) l3))))
        (at l3 (println (+ x y))))))

(test-case
 "Reference let binding out of scope"
 (check-has-syntax-error
  (require choret)
  (chor (l1 l2 l3)
        (define (at l3 x) 0)
        (define (at l3 y) 0)
        (let ([(at l1 x) 10])
          (set! (at l3 x) (~> (at l1 x) l3))
          (let ([(at l2 y) 15])
            (set! (at l3 y) (~> (at l2 y) l3))))
        (at l1 (println x))
        (at l3 (println (+ x y))))))

(test-case
 "Simple choreographic function"
 (check-not-syntax-error
  (require choret rackunit)
  (chor (l1 l2)
        (define F
          (lambda (X)
            (let ([(at l1 x) X])
              (let ([(at l2 x) (~> (at l1 x) l2)])
                (at l2 (+ x 10))))))

        (let ([(at l2 result)
               (F (at l1 10))])
          (at l2 (at l2 (println result)))
          (at l2 (check-equal? result 20))))))

(test-case
 "Passing a chor function to a chor function"
 (check-not-syntax-error
  (require choret rackunit)
  (chor (l1 l2)
        (define F
          (lambda (F2)
            (F2 (at l2 10) (at l1 20))))

        (define add-10
          (lambda (X (at l1 y))
            (let ([(at l2 y) (~> (at l1 y) l2)])
              (at l2 (+ X y)))))

        (let ([(at l2 result) (F add-10)])
          (at l2 (check-equal? result 30))))))

(test-case
 "Merging with choreographic functions"
 (check-not-syntax-error
  (require choret rackunit)
  (chor (l1 l2)
        (if (at l1 #t)
            (let ([(at l2 x) (at l2 5)])
              (define F
                (lambda (X)
                  (unless (void? X) (println X))))
              (F (at l2 x)))
            (let ([(at l2 x) (at l2 5)])
              (define F
                (lambda (X)
                  (unless (void? X) (println X))))
              (F (at l2 x)))))))

(test-case
 "Context based expansion of Choret macros"
 (check-not-syntax-error
  (require choret rackunit)
  (define regular-func (lambda (x y) (+ x y)))
  (chor (l1 l2)
        (define (at l1 x) (at l1 10))
        (define chor-func
          (lambda (X) (at l1 (regular-func X x))))
        (let ([(at l1 result) (chor-func (at l1 10))])
          (at l1 (check-equal? result 20))))))

(test-case
 "Context based expansion of Choret macros, 2"
 (check-has-syntax-error
  (at l1 10)
  (chor (l1 l2)
        (at l1 (println 0)))))

(test-case
 "Context based expansion of Choret macros, 3"
 (check-has-syntax-error
  (let ([(at l1 x) (at l1 10)])
    (void))
  (chor (l1 l2)
        (at l1 (println x)))))
