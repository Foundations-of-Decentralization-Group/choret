#lang racket/base

(require (for-template "spl-type-forms.rkt"))
(require racket/class syntax/parse)
(require (for-syntax racket/base racket/syntax syntax/parse))

(provide
 parse-type eq/type? eq/type-lambda?
 (struct-out spl-type) (struct-out lambda-type))

(struct spl-type [kind value])
(struct lambda-type [arg-types ret-type])

;; This macro is overkill; I just implemented it this way to experiment.
(define-syntax (define/provide-primitive-types stx)
  (syntax-parse stx
    [((~literal define/provide-primitive-types)
      types ...)
     (define/syntax-parse (types^ ...)
       (for/list ([name (syntax->list #'(types ...))])
         (define/syntax-parse name-type
           (format-id stx "~a-type" name))
         (define/syntax-parse name-type-check
           (format-id stx "~a-type?" name))
         #`(begin
             (define name-type
               (spl-type '#,name #f))
             (define (name-type-check type)
               (eq? (spl-type-kind type) '#,name))
             (provide name-type name-type-check))))
     #'(begin types^ ...)]))

(define/provide-primitive-types
  invalid int bool)



(define (parse-type stx)
  (syntax-parse stx #:literal-sets (spl-type-literals)
    [int int-type]
    [bool bool-type]
    [(-> (arg-types ...) ret-type)
     (spl-type
      'lambda
      (lambda-type
       (map parse-type (syntax->list #'(arg-types ...)))
       (parse-type #'ret-type)))]
    [_ invalid-type]))

(define (eq/type? t1 t2)
  (if (not (and (spl-type? t1) (spl-type? t2)))
      #f
      (let ([t1-kind (spl-type-kind t1)] [t2-kind (spl-type-kind t2)])
        (and
         (eq? t1-kind t2-kind)
         (if (eq? t1-kind 'lambda)
             (eq/type-lambda? t1 t2)
             #t)))))

(define (eq/type-lambda? t1 t2)
  (let ([t1-arg-types (lambda-type-arg-types (spl-type-value t1))]
        [t1-ret-type (lambda-type-ret-type (spl-type-value t1))]
        [t2-arg-types (lambda-type-arg-types (spl-type-value t2))]
        [t2-ret-type (lambda-type-ret-type (spl-type-value t2))])
    (and
     (foldl
      (lambda (t1-arg t2-arg acc) (and acc (eq/type? t1-arg t2-arg)))
      #t
      t1-arg-types
      t2-arg-types)
     (eq/type? t1-ret-type t2-ret-type))))
  
