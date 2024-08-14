#lang racket/base

(require
 racket/stxparam
  (for-syntax
   racket/base
   syntax/parse))

(provide in-global-expr define/provide-chor-syntax)

(define-syntax-parameter in-global-expr #f)

(define-syntax (define/provide-chor-syntax stx)
  (syntax-parse stx
    [(_ (NAME STX) (~optional (~seq #:override OVERRIDE)) BODY ...)
     #`(begin
         #,(if (attribute OVERRIDE)
               #'(provide (rename-out [NAME OVERRIDE]))
               #'(provide NAME))
         (define-syntax (NAME STX)
           (if (syntax-parameter-value #'in-global-expr)
               (let () BODY ...)
               #,(if (attribute OVERRIDE)
                     #`(syntax-case STX ()
                         [(... (_ ARG ...))
                          (syntax (#,(attribute OVERRIDE) ARG (... ...)))])
                     #'(raise-syntax-error
                        #f
                        (string-append
                         "Cannot use a chor form outside a choreographic "
                         "expression.")
                        STX)))))]))
