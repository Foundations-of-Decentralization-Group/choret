#lang s-exp "simple-chor-ee-lib.rkt"

(require rackunit)

(define-syntax my-com->
  (chor-macro
   (lambda (stx)
     (syntax-case stx ()
       [(_ sender reciever)
        #'(com-> sender reciever)]))))

(define-chor [A B C D]
  (define-local A x 12)
  (my-com-> [A 10] [C c1])               ;; A.10 -> C.c1
  (expr-local C (check-equal? c1 10))
  (com-> [B 100] [C c2])              ;; B.100 -> C.c2
  (expr-local C (check-equal? c2 100))
  (com-> [C (+ c1 c2)] [D d1])        ;; C.(c1 + c2) -> D.d1
  (expr-local D (check-equal? d1 110))
  (com-> [B 100] [D d2])              ;; B.100 -> D.d2
  (expr-local D (check-equal? d2 100))
  (com-> [D (+ d1 d2)] [C c3])        ;; D.(d1 + d2) -> C.c3
  (expr-local C (check-equal? c3 210))
  (com-> [C (+ c1 c2 c3)] [A a])      ;; C.(c1 + c2 + c3) -> A.a
  (expr-local A (check-equal? a 320))
  (expr-local A (printf "Final Result: ~a\n" a))) ;; Print final result at A
