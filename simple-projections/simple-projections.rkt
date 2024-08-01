#lang racket/base

(require
 ee-lib/define
 "../simple-networks/simple-networks.rkt"
 (for-syntax
  racket/base racket/list racket/hash ee-lib syntax/parse))

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

  (define (matching-names? name1 name2)
    (unless (free-identifier=? name1 name2)
      (raise-syntax-error
       #f
       "Cannot merge due to mismatched process names!"
       #f
       #f
       (list name1 name2))))

  (define (matching-local-exprs? expr1 expr2)
    (unless (equal? (syntax->datum expr1) (syntax->datum expr2))
      (raise-syntax-error
       #f
       "Cannot merge mismatched local expressions!"
       #f
       #f
       (list expr1 expr2))))

  (define (matching-labels? label1 label2)
    (unless (equal? (syntax->datum label1) (syntax->datum label2))
      (raise-syntax-error
       #f
       "Cannot merge mismatched labels!"
       #f
       #f
       (list label1 label2))))

  (define (merge expr1 expr2)
    (syntax-parse #`(#,expr1 #,expr2) #:literal-sets (proj-literals)
      [((send!/proj NAME1 EXPR1) (send!/proj NAME2 EXPR2))
       (matching-names? #'NAME1 #'NAME2)
       (matching-local-exprs? #'EXPR1 #'EXPR2)
       #'(send!/proj NAME1 EXPR1)]

      [((recv?/proj NAME1 ID1) (recv?/proj NAME2 ID2))
       (matching-names? #'NAME1 #'NAME2)
       (matching-local-exprs? #'ID1 #'ID2)
       #'(recv?/proj NAME1 ID1)]

      [((begin/proj EXPR1 ...) (begin/proj EXPR2 ...))
       (define expr1-list (syntax->list #'(EXPR1 ...)))
       (define expr2-list (syntax->list #'(EXPR2 ...)))
       (let ([expr1-len (length expr1-list)] [expr2-len (length expr2-list)])
         (unless (equal? expr1-len expr2-len)
           (raise-syntax-error
            #f
            "Cannot merge begin/chor forms with different ammounts of subforms!"
            (if (> expr1-len expr2-len) (last expr1-list) (last expr2-list)))))
       #`(begin/proj
           #,@
           (for/list ([expr1 expr1-list] [expr2 expr2-list])
             (merge expr1 expr2)))]

      [((expr-local/proj EXPR1) (expr-local/proj EXPR2))
       (matching-local-exprs? #'EXPR1 #'EXPR2)
       #'(expr-local/proj EXPR1)]

      [((if/proj COND1 EXPR-TRUE1 EXPR-FALSE1)
        (if/proj COND2 EXPR-TRUE2 EXPR-FALSE2))
       (matching-local-exprs? #'COND1 #'COND2)
       #`(if/proj COND1
                  #,(merge #'EXPR-TRUE1 #'EXPR-TRUE2)
                  #,(merge #'EXPR-FALSE1 #'EXPR-FALSE2))]

      [((select!/proj NAME1 LABEL1) (select!/proj NAME2 LABEL2))
       (matching-names? #'NAME1 #'NAME2)
       (matching-labels? #'LABEL1 #'LABEL2)
       #'(select!/proj NAME1 LABEL1)]

      [((branch?/proj NAME1 [LABEL1 EXPR1] ...)
        (branch?/proj NAME2 [LABEL2 EXPR2] ...))
       (matching-names? #'NAME1 #'NAME2)
       (define label-hash1
         (make-immutable-hash
          (for/list ([label1 (map syntax->datum (syntax->list #'(LABEL1 ...)))]
                     [expr1 (syntax->list #'(EXPR1 ...))])
            (cons label1 expr1))))
       (define label-hash2
         (make-immutable-hash
          (for/list ([label2 (map syntax->datum (syntax->list #'(LABEL2 ...)))]
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
                    #`[#,label #,expr])))))]

      [(EXPR1 EXPR2)
       (raise-syntax-error
        #f
        "Could not merge!"
        #f
        #f
        (list #'EXPR1 #'EXPR2))]))

  (define (parse-expr stx)
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
       #`(let ([recv-label (recv NAME 'void)])
           (cond
             #,@
             (for/list ([label (syntax->list #'(LABEL ...))]
                        [expr (syntax->list #'(EXPR ...))])
               #`[(eq? recv-label #,label) #,(parse-expr expr)])))]))

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
