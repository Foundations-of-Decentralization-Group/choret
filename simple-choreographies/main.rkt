#lang s-exp "simple-choreographies.rkt"

(require racket/base)

(define-chor [A B C D]
  (com-> [A 10] [C c1])               ;; A.10 -> C.c1
  (com-> [B 100] [C c2])              ;; B.100 -> C.c2
  (com-> [C (+ c1 c2)] [D d1])        ;; C.(c1 + c2) -> D.d1
  (com-> [B 100] [D d2])              ;; B.100 -> D.d2
  (com-> [D (+ d1 d2)] [C c3])        ;; D.(d1 + d2) -> C.c3
  (com-> [C (+ c1 c2 c3)] [A a])      ;; C.(c1 + c2 + c3) -> A.a
  (local-expr A (printf "Final Result: ~a\n" a))) ;; Print final result at A
