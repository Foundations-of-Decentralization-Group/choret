#lang s-exp "../simple-projections.rkt"

;; (define-local B b 0)
;; (define-local C c 0)
;; (define-local D d1 0)
;; (define-local D d2 0)
;; (define-local D res)

;; (com-> [A 10] [B b])
;; (com-> [A 20] [C c])
;; (com-> [B b] [D d1])
;; (com-> [C c] [D d2])
;; (expr-local D (set! res (+ d1 d2)))

(simple-projections

 (define-projection A
   (send!/proj B 10)
   (send!/proj C 20))

 (define-projection B
   (define/proj b 0)
   (recv?/proj A b)
   (send!/proj D b))

 (define-projection C
   (define/proj c 0)
   (recv?/proj A c)
   (send!/proj D c))

 (define-projection D
   (define/proj d1 0)
   (define/proj d2 0)
   (define/proj res 0)
   (recv?/proj B d1)
   (recv?/proj C d2)
   (expr-local/proj
    (begin
      (set! res (+ d1 d2))
      (printf "d1: ~a, d2: ~a, res: ~a\n" d1 d2 res)))))
