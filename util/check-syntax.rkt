#lang racket/base

(require
  rackunit
  (for-syntax
   racket/base
   rackunit))

(provide
 check-not-syntax-error
 check-has-syntax-error)

(define-syntax (check-not-syntax-error stx)
  (syntax-case stx ()
    [(_ BODY ...)
     #`(begin
         (with-handlers ([exn:fail:syntax?
                          (lambda (exn)
                            (fail (format "Syntax error: ~a" exn)))])
           (parameterize ([current-namespace (make-base-namespace)])
             (expand (quote (module test racket/base BODY ...)))
             (eval
              (quote
               (begin
                 (module test racket/base BODY ...)
                 (require 'test))))))
         (void))]))

(define-syntax (check-has-syntax-error stx)
  (syntax-case stx ()
    [(_ BODY ...)
     #`(begin
         (current-load-relative-directory #,(current-directory))
         (with-handlers ([exn:fail:syntax?
                          (lambda (_) (void))])
           (parameterize ([current-namespace (make-base-namespace)])
             (expand (quote (module test racket/base BODY ...)))
             (fail "Expected a syntax error!")))
         (void))]))
