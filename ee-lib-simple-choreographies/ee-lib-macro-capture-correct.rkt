 #lang s-exp "simple-chor-ee-lib.rkt"

(define-chor [A]
  (local-define A x 12)

  (define-syntax/chor print-plus-x
    (lambda (stx)
      (syntax-case stx ()
        [(_ P v)
         #'(local-expr P (println (+ x v)))])))

  (print-plus-x A 2))
