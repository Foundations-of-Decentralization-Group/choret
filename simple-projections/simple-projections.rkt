#lang racket/base

(require
 ee-lib/define
 (except-in "../simple-networks/simple-networks.rkt" #%module-begin)
 (for-syntax
  racket/base racket/hash ee-lib syntax/parse))

(provide
 (all-from-out racket/base)
 ;; Interface Macro
 simple-projections
 ;; Literal forms
 define-projection
 define/proj begin/proj expr-local/proj
 send!/proj recv?/proj
 merge/proj if/proj select!/proj branch?/proj)

(define-literal-forms proj-literals
  "Cannot use a simple-projections literal in Racket!"
  (define-projection #%define-projection
   define/proj begin/proj expr-local/proj
   send!/proj recv?/proj
   merge/proj if/proj select!/proj branch?/proj))

(begin-for-syntax
  (struct proj-variable [])

  (define current-process (make-parameter #f))

  (define (valid-other-process? name [ctx name])
    (unless (lookup name)
      (raise-syntax-error #f "Undefined process name" name))
    (when (free-identifier=? name (current-process))
      (raise-syntax-error
       #f
       "Cannot send!/recv?/select!/branch? on the current process!"
       ctx)))

  (define (merge expr1 expr2)
    (syntax-parse #`(#,expr1 #,expr2) #:literal-sets (proj-literals)
      [((send!/proj NAME1 EXPR1) (send!/proj NAME2 EXPR2))
       (if (and
            (eq? (syntax->datum #'NAME1) (syntax->datum #'NAME2))
            (eq? (syntax->datum #'EXPR1) (syntax->datum #'EXPR2)))
           #'(send!/proj NAME1 EXPR1)
           (raise-syntax-error #f "Could not merge!" this-syntax))]

      [((begin/proj EXPR1 ...) (begin/proj EXPR2 ...))
       #`(begin/proj
           #,@
           (for/list ([expr1 (syntax->list #'(EXPR1 ...))]
                      [expr2 (syntax->list #'(EXPR2 ...))])
             (merge expr1 expr2)))]

      [((branch?/proj NAME1 [LABEL1 EXPR1] ...)
        (branch?/proj NAME2 [LABEL2 EXPR2] ...))
       (unless (eq? (syntax->datum #'NAME1) (syntax->datum #'NAME2))
         (raise-syntax-error
          #f
          "Branches cannot merge due to mismatched process names!"
          this-syntax))
       (define label-hash1
         (make-immutable-hash
          (for/list ([label1 (syntax->list #'(LABEL1 ...))]
                     [expr1 (syntax->list #'(EXPR1 ...))])
            (cons label1 expr1))))
       (define label-hash2
         (make-immutable-hash
          (for/list ([label2 (syntax->list #'(LABEL2 ...))]
                     [expr2 (syntax->list #'(EXPR2 ...))])
            (cons label2 expr2))))
       (define label-union
         (hash-union
          label-hash1
          label-hash2
          #:combine/key (lambda (_ a b) (cons a b))))
       #`(branch?/proj
          NAME1
          #,@
          (for/list ([pair (hash->list label-union)])
            (let ([label (car pair)]
                  [expr (cdr pair)])
              (if (pair? expr)
                  ;; The case where a label is in both branches being merged
                  (let* ([expr1 (car expr)]
                         [expr2 (cdr expr)]
                         [merged-expr (merge expr1 expr2)])
                    #`[#,label #,merged-expr])
                  ;; The case where a label is in only one branch
                  (begin
                    #`[#,label #,expr])))))]))

  (define/hygienic (parse-expr stx) #:definition
    (syntax-parse stx #:literal-sets (proj-literals)
      [(send!/proj NAME GEN-EXPR)
       (valid-other-process? #'NAME this-syntax)
       #'(send NAME 'void GEN-EXPR)]

      [(recv?/proj NAME ID)
       (valid-other-process? #'NAME this-syntax)
       #'(set! ID (recv NAME 'void))]

      [(define/proj NAME EXPR)
       #'(define NAME EXPR)]

      [(begin/proj BODY ...)
       #`(begin
           #,@
           (for/list ([expr (syntax->list #'(BODY ...))])
             (parse-expr expr)))]

      [(expr-local/proj EXPR)
       #'EXPR]

      [(merge/proj EXPR1 EXPR2)
       (parse-expr (merge #'EXPR1 #'EXPR2))]

      [(if/proj COND EXPR-TRUE EXPR-FALSE)
       #`(if COND
             #,(parse-expr #'EXPR-TRUE)
             #,(parse-expr #'EXPR-FALSE))]

      [(select!/proj NAME LABEL)
       (valid-other-process? #'NAME this-syntax)
       #`(send NAME 'void LABEL)]

      [(branch?/proj NAME [LABEL EXPR] ...)
       (valid-other-process? #'NAME this-syntax)
       #`(cond
           #,@
           (for/list ([label (syntax->list #'(LABEL ...))]
                      [expr (syntax->list #'(EXPR ...))])
             #`[(eq? (recv NAME 'void) #,label) #,(parse-expr expr)]))]))

  (define (parse-intctx-pass2 stx)
    #`(begin
        #,@
        (for/list ([stmt (syntax->list stx)])
          (syntax-parse stmt #:literal-sets (proj-literals)
            [(#%define-projection NAME BODY ...)
             #`(define-process NAME
                 #,@
                 (parameterize ([current-process #'NAME])
                   (for/list ([expr (syntax->list #'(BODY ...))])
                     (parse-expr expr))))]))))

  (define (parse-intctx-pass1 stx)
    #`(#,@
       (for/list ([stmt (syntax->list stx)])
         (syntax-parse stmt #:literal-sets (proj-literals)
           [(define-projection NAME BODY ...)
            #`(#%define-projection
               #,(syntax-local-introduce-splice
                  (bind! #'NAME (proj-variable))) BODY ...)]))))

  (define/hygienic (parse-intctx stx) #:definition
    (with-scope sc (parse-intctx-pass2 (parse-intctx-pass1 stx)))))

(define-syntax (simple-projections stx)
  (syntax-parse stx
    [(_ BODY ...)
     #`(define-network
         #,(parse-intctx #'(BODY ...)))]))
