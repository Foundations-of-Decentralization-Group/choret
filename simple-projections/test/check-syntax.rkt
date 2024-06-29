#lang racket/base

(require syntax/macro-testing rackunit)

(provide check-not-syntax-error check-has-syntax-error)

(define-syntax-rule (check-not-syntax-error BODY ...)
  (with-handlers ([exn:fail:syntax?
                   (lambda (exn)
                     (fail (format "Syntax error: ~a" exn)))])
    (phase1-eval
     BODY ...
     #:catch? #t)))

(define-syntax-rule (check-has-syntax-error BODY ...)
  (with-handlers ([exn:fail:syntax? (lambda (exn) (void))])
    (phase1-eval
     BODY ...
     #:catch? #t)
    (fail "Expected a syntax error!")))
