#lang racket/base

(require rackunit
         "../util/check-syntax.rkt"
         (for-syntax racket/base))

;; (test-example FILE)
;; Generates a test for a given test in the "examples" directory with the file
;; name specified by the string FILE.
(define-syntax (test-example stx)
  (syntax-case stx ()
    [(_ FILE)
     #`(test-case
        #,(format "Example: ~a" (syntax->datum #'FILE))
        (check-not-syntax-error
         (require #,(format "../examples/~a" (syntax->datum #'FILE)))))]))


(test-example "book-seller.rkt")
(test-example "factorial.rkt")
