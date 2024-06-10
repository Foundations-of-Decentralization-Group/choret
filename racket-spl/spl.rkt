#lang racket/base

(require ee-lib/define
         (for-syntax ee-lib syntax/parse racket/base))

(define-literal-forms spl-literals "Cannot use an spl form in Racket."
    (let/spl lambda/spl seq/spl add-int/spl and/spl println/spl int bool ->))

(provide let/spl lambda/spl seq/spl add-int/spl and/spl println/spl int bool ->)
(provide (except-out (all-from-out racket/base) #%module-begin))

(begin-for-syntax
  (struct spl-variable [type])
  (struct spl-type [kind value])
  (struct lambda-type [arg-types ret-type])

  (define (parse-type stx)
    (syntax-parse stx #:literal-sets (spl-literals)
      [int (spl-type 'int #f)]
      [bool (spl-type 'bool #f)]
      [(-> (arg-types ...) ret-type)
       (spl-type
        'lambda
        (lambda-type
         (map parse-type (syntax->list #'(arg-types ...)))
         (parse-type #'ret-type)))]
      [_ (spl-type #f #f)]))

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

  (define/hygienic (spl-expand stx) #:expression
    (syntax-parse stx #:literal-sets (spl-literals)
      [num-lit:number
       (syntax-property
        #'num-lit
        'type
        (spl-type 'int #f))]


      [bool-lit:boolean
       (syntax-property
        #'bool-lit
        'type
        (spl-type 'bool #f))]


      [var-use:id
       #:when (lookup #'var-use spl-variable?)
       (define var (lookup #'var-use))
       (syntax-property
        #'var-use
        'type
        (spl-variable-type var))]


      [(func:id arg:expr ...)
       #:when (lookup #'func spl-variable?)
       (define var (lookup #'func))
       (define func-type (spl-variable-type var))
       (define func-type-kind (spl-type-kind func-type))
       (unless (eq? func-type-kind 'lambda)
         (raise-syntax-error
          #f
          "Not bound to a lambda function!"
          #'func))
       (define func-type-val (spl-type-value func-type))
       (define func-arg-types (lambda-type-arg-types func-type-val))
       (define func-ret-type (lambda-type-ret-type func-type-val))
       (define/syntax-parse
         args^ #`(#,@(for/list ([i (syntax->list #'(arg ...))])
                       (spl-expand i))))
       (define arg-types (for/list ([i (syntax->list #'args^)])
                           (syntax-property i 'type)))
       (for/list ([actual arg-types] [expected func-arg-types])
         (unless (eq/type? actual expected)
           (raise-syntax-error
            #f
            (format "Actual type ~a does not match expected type ~a!"
                    (spl-type-kind actual)
                    (spl-type-kind expected)))))
       (syntax-property
        #'(func (~@ . args^))
        'type
        func-ret-type)]


      [(let/spl (type var-name:id val-exp:expr) sub-exp:expr)
       (with-scope sc
         (define/syntax-parse val-exp^ (spl-expand #'val-exp))
         (define val-exp-type (syntax-property #'val-exp^ 'type))
         (unless (eq/type? val-exp-type (parse-type #'type))
           (raise-syntax-error #f "Expression does not match type!" #'val-exp^))
         (define/syntax-parse
           var-name^ (bind! #'var-name (spl-variable val-exp-type)))
         (define/syntax-parse sub-exp^ (spl-expand #'sub-exp))
         (define sub-exp-type (syntax-property #'sub-exp^ 'type))
         (syntax-property
          #'(let ([var-name^ val-exp^]) sub-exp^)
          'type
          sub-exp-type))]


      [(lambda/spl ([types args:id] ...) ret-type body:expr)
       (with-scope sc
         (define/syntax-parse
           args^ #`(#,@(for/list ([i (syntax->list #'([types args] ...))])
                         (with-syntax ([(typ arg) i])
                           (bind! #'arg (spl-variable (parse-type #'typ)))))))
         (define arg-types (for/list ([typ (syntax->list #'(types ...))])
                             (parse-type typ)))
         (define ret-typ (parse-type #'ret-type))
         (define/syntax-parse body^ (spl-expand #'body))
         (define body-type (syntax-property #'body^ 'type))
         (unless (eq/type? body-type ret-typ)
           (raise-syntax-error
            #f
            "Return type does not match body type!"
            this-syntax))
         (syntax-property
          #'(lambda args^ body^)
          'type
          (spl-type 'lambda (lambda-type arg-types ret-typ))))]


      [(seq/spl first:expr second:expr)
       (define/syntax-parse first^ (spl-expand #'first))
       (define/syntax-parse second^ (spl-expand #'second))
       (define second-type (syntax-property #'second^ 'type))
       (syntax-property
        #'(begin first^ second^)
        'type
        second-type)]


      [(add-int/spl first:expr second:expr)
       (define/syntax-parse first^ (spl-expand #'first))
       (define/syntax-parse second^ (spl-expand #'second))
       (define first-type (syntax-property #'first^ 'type))
       (define second-type (syntax-property #'second^ 'type))
       (unless (and
                (eq? (spl-type-kind first-type) 'int)
                (eq? (spl-type-kind second-type) 'int))
         (raise-syntax-error #f "Both arguments must be ints!" this-syntax))
       (syntax-property
        #'(+ first^ second^)
        'type
        (spl-type 'int #f))]


      [(and/spl first:expr second:expr)
       (define/syntax-parse first^ (spl-expand #'first))
       (define/syntax-parse second^ (spl-expand #'second))
       (define first-type (syntax-property #'first^ 'type))
       (define second-type (syntax-property #'second^ 'type))
       (unless (and
                (eq? (spl-type-kind first-type) 'bool)
                (eq? (spl-type-kind second-type) 'bool))
         (raise-syntax-error #f "Both arguments must be bools!" this-syntax))
       (syntax-property
        #'(and first^ second^)
        'type
        (spl-type 'bool #f))]


      [(println/spl arg:expr)
       (define/syntax-parse arg^ (spl-expand #'arg))
       (syntax-property
        #'(println arg^)
        'type
        (spl-type #f #f))])
    )
  )

(define-syntax (#%spl-module-begin stx)
  (syntax-parse stx
    [(_ body)
     (with-scope sc (spl-expand #'body))]))

(define-syntax (spl-module-begin stx)
  (syntax-parse stx
    [(_ body)
     #'(#%module-begin (#%spl-module-begin body))]))

(provide (rename-out [spl-module-begin #%module-begin]))
