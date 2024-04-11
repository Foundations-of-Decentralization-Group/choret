#lang s-exp "simple-networks.rkt"

(require racket/base)

(define-network

  (define-process A
    ;; [C!; C?]
    (send C 'int 10)
    (printf "Final value: ~a\n" (recv C 'int)))

  (define-process B
    ;; [C!; D!;]
    (send C 'int 100)
    (send D 'int 100))

  (define-process C
    ;; [A?; B?; D!; D?; A!;]
    (define sum1 (+ (recv A 'int) (recv B 'int)))
    (send D 'int sum1)
    (define sum2 (+ sum1 (recv D 'int)))
    (send A 'int sum2))

  (define-process D
    ;; [C?; B?; C!;]
    (define sum1 (+ (recv C 'int) (recv B 'int)))
    (send C 'int sum1)))
