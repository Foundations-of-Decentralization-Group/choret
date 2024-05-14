#lang s-exp "../simple-choreographies.rkt"

(require racket/base)
(require rackunit)

(define-chor [A B C D]
  (com-> [A 10] [C c1])               ;; A.10 -> C.c1
  (local-expr C (check-equal? c1 10))
  (com-> [B 100] [C c2])              ;; B.100 -> C.c2
  (local-expr C (check-equal? c2 100))
  (com-> [C (+ c1 c2)] [D d1])        ;; C.(c1 + c2) -> D.d1
  (local-expr D (check-equal? d1 110))
  (com-> [B 100] [D d2])              ;; B.100 -> D.d2
  (local-expr D (check-equal? d2 100))
  (com-> [D (+ d1 d2)] [C c3])        ;; D.(d1 + d2) -> C.c3
  (local-expr C (check-equal? c3 210))
  (com-> [C (+ c1 c2 c3)] [A a])      ;; C.(c1 + c2 + c3) -> A.a
  (local-expr A (check-equal? a 320))
  (local-expr A (printf "Final Result: ~a\n" a))) ;; Print final result at A
