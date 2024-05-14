#lang s-exp "../simple-choreographies.rkt"

;; An example based on the key exchange protocol choreography listed on pages
;; 96 and 97 of the book "Introduction To Choreographies" by Fabrizio Montesi.
;; The primitive root (the value in "g") was taken from a table of primitive
;; roots on Wikipedia:
;; https://en.wikipedia.org/wiki/Primitive_root_modulo_n

(require racket/base)
(require rackunit)

(define-chor [Alice Bob]
  (local-expr Alice
              (define p 23)
              (define g 17)
              (define a 562)
              (define (modPow base expn m)
                (modulo (expt base expn) m)))
  (local-expr Bob
              (define p 23)
              (define g 17)
              (define b 341)
              (define (modPow base expn m)
                (modulo (expt base expn) m)))

  (com-> [Alice (modPow g a p)] [Bob x])
  (com-> [Bob (modPow g b p)] [Alice y])

  (local-expr Alice
              (check-equal? (modPow y a p) 1)
              (printf "Secret key at Alice: ~a\n" (modPow y a p)))
  (local-expr Bob
              (check-equal? (modPow x b p) 1)
              (printf "Secret key at Bob: ~a\n" (modPow x b p))))
