#lang racket/base

(require
 "define-chor-syntax.rkt"
 "threads-network.rkt"
 racket/stxparam
 racket/block
 (for-syntax
  racket/base
  racket/syntax
  racket/hash
  syntax/parse
  (for-syntax
   racket/base)))

(provide chor)

(begin-for-syntax

  (define (cur-process? proc)
    (free-identifier=? (syntax-parameter-value #'cur-process) proc))

  (define (eq-process? proc1 proc2)
    (free-identifier=? proc1 proc2))

  (define (local-expand-expr stx)
    (let-values ([(expanded-stx) (local-expand stx 'expression '())])
      expanded-stx)))

(define-syntax-parameter cur-process
  (lambda (stx)
    (raise-syntax-error
     #f
     "Use of `cur-process` outside of a projection!"
     stx)))

(define/provide-chor-syntax (at stx)
  (syntax-parse stx
    [(_ PROC LEXPR)
     (if (cur-process? #'PROC)
         #'LEXPR
         #'(void))]))

(define/provide-chor-syntax (let/chor stx) #:override let
  (syntax-parse stx
    [(_ ([AT-EXPR VAL-GEXPR] ...) GBODY ...)
     (let ([bindings
            (filter
             (lambda (x) x)
             (for/list ([at-expr (syntax->list #'(AT-EXPR ...))]
                        [val-expr (syntax->list #'(VAL-GEXPR ...))])
               (syntax-parse at-expr #:literals (at)
                 [(at PROC LEXPR)
                  (if (cur-process? #'PROC)
                      #`[LEXPR #,val-expr]
                      #`[#,(gensym 'IGNORE-) #,val-expr])]
                 [_ (raise-syntax-error
                     #f "Expected located expression!" at-expr)])))])
       #`(let #,bindings GBODY ...))]))

(define/provide-chor-syntax (define/chor stx) #:override define
  (syntax-parse stx #:literals (at)
    [(_ (at PROC LEXPR) BODY-GEXPR ...)
     (if (cur-process? #'PROC)
         #'(define LEXPR BODY-GEXPR ...)
         #'(let () BODY-GEXPR ...))]
    [(_ ID GEXPR)
     #'(define ID GEXPR)]))

(define/provide-chor-syntax (set!/chor stx) #:override set!
  (syntax-parse stx #:literals (at)
    [(_ (at PROC LEXPR) GEXPR)
     (if (cur-process? #'PROC)
         #'(set! LEXPR GEXPR)
         #'(let () GEXPR))]
    [(_ ID GEXPR)
     #'(set! ID GEXPR)]))

(define/provide-chor-syntax (lambda/chor stx) #:override lambda
  (syntax-parse stx
    [(_ (GARG ...) GBODY ...)
     (let ([args
            (for/list ([garg (syntax->list #'(GARG ...))])
              (syntax-parse garg #:literals (at)
                [(at PROC LEXPR)
                 (if (cur-process? #'PROC)
                     #'LEXPR
                     (format-id #'LEXPR "UNUSED-located-at-~a" #'PROC))]
                [ARG #'ARG]))])
       #`(lambda #,args
           GBODY ...))]))

(define/provide-chor-syntax (if/chor stx) #:override if
  (syntax-parse stx #:literals (at)
    [(_ (at PROC LEXPR) TRUE-GEXPR FALSE-GEXPR)
     (if (cur-process? #'PROC)
         #'(if LEXPR TRUE-GEXPR FALSE-GEXPR)
         (merge
          (local-expand-expr #'TRUE-GEXPR)
          (local-expand-expr #'FALSE-GEXPR)))]
    [(_ REST ...)
     #'(if REST ...)]))

(define/provide-chor-syntax (~> stx)
  (syntax-parse stx #:literals (at)
    [(_ (at SEND-PROC SEND-LEXPR) RECV-PROC)
     (cond [(cur-process? #'SEND-PROC)
            #'(send RECV-PROC 'void SEND-LEXPR)]
           [(cur-process? #'RECV-PROC)
            #`(recv SEND-PROC 'void)]
           [else #'(void)])]))

(define/provide-chor-syntax (sel~> stx)
  (syntax-parse stx
    [(_ SEND-PROC [RECV-PROC LABEL GEXPR] ...)
     (if (cur-process? #'SEND-PROC)
         #'(begin (choose! RECV-PROC LABEL GEXPR) ...)
         #`(begin
             #,@(filter
                 (lambda x x)
                 (for/list ([recv-proc (syntax->list #'(RECV-PROC ...))]
                            [gexpr (syntax->list #'(GEXPR ...))]
                            [label (syntax->list #'(LABEL ...))])
                   (if (eq-process? #'SEND-PROC recv-proc)
                       (raise-syntax-error
                        #f
                        "Process cannot send a selection to itself!"
                        #'SEND-PROC
                        recv-proc)
                       (if (cur-process? recv-proc)
                           #`(branch?
                              SEND-PROC [#,label #,gexpr])
                           #f))))))]))

(define-syntax (choose! stx)
  (syntax-parse stx
    [(_ RECV-PROC LABEL GEXPR)
     #'(begin
         (send RECV-PROC 'void LABEL)
         GEXPR)]))

(define-for-syntax (merge-branches left-branch right-branch)
  (syntax-parse #`(#,left-branch #,right-branch)
    [((_ L-SEND-PROC [L-LABEL L-GEXPR] ...)
      (_ R-SEND-PROC [R-LABEL R-GEXPR] ...))
     (unless (equal? (syntax->datum #'L-SEND-PROC)
                     (syntax->datum #'R-SEND-PROC))
       (raise-syntax-error
        #f
        "Selections with mismatched senders cannot be merged!"
        #'L-SEND-PROC))

     (define l-label-hash
       (make-immutable-hash
        (for/list ([l-label (map syntax->datum (syntax->list #'(L-LABEL ...)))]
                   [l-expr (syntax->list #'(L-GEXPR ...))])
          (cons l-label l-expr))))
     (define r-label-hash
       (make-immutable-hash
        (for/list ([r-label (map syntax->datum (syntax->list #'(R-LABEL ...)))]
                   [r-expr (syntax->list #'(R-GEXPR ...))])
          (cons r-label r-expr))))
     (define label-union
       (hash-union
        l-label-hash
        r-label-hash
        #:combine/key (lambda (_ a b) (cons a b))))
     (syntax-property
      #`(quote-syntax
         (branch?
          L-SEND-PROC
          #,@
          (for/list ([pair (hash->list label-union)])
            (let ([label (car pair)]
                  [expr (cdr pair)])
              (if (pair? expr)
                  ;; The case where a label is in both branches being merged
                  #`[#,label #,(merge (car expr) (cdr expr))]
                  ;; The case where a label is in only one branch
                  #`[#,label #,expr]))))
         #:local)
      'branch
      #t)]))

(define-syntax (branch? stx)
  (syntax-parse stx #:literals (branch?)
    [(branch? SEND-PROC [LABEL GEXPR] ...)
     (syntax-property
      #`(quote-syntax
         (branch?
          SEND-PROC
          #,@(for/list ([label (syntax->list #'(LABEL ...))]
                        [gexpr (syntax->list #'(GEXPR ...))])
               #`[#,label
                  #,(local-expand-expr gexpr)]))
         #:local)
      'branch
      #t)]))

(define-for-syntax (merge left-stx right-stx)
  (syntax-parse #`(#,left-stx #,right-stx) #:literals (quote-syntax branch?)
    [((quote-syntax (branch? L-REST ...) #:local)
      (quote-syntax (branch? R-REST ...) #:local))
     #:when (and
             (syntax-property left-stx 'branch)
             (syntax-property left-stx 'branch))
     (merge-branches #'(branch? L-REST ...) #'(branch? R-REST ...))]
    [((L ...) (R ...))
     (let ([l-list (syntax->list #'(L ...))]
           [r-list (syntax->list #'(R ...))])
       (if (equal? (length l-list) (length r-list))
           #`(#,@(map merge l-list r-list))
           (raise-syntax-error
            #f
            "Cannot merge mismatched number of expressions!"
            left-stx)))]
    [(L R)
     (if (equal? (syntax->datum #'L) (syntax->datum #'R))
         #'L
         (raise-syntax-error
          #f
          "Cannot merge mismatched expressions."
          left-stx))]))

(define-for-syntax (expand-branches stx)
  (syntax-parse stx #:literals (quote-syntax branch?)
    [(quote-syntax (branch? SEND-PROC (LABEL EXPR) ...) #:local)
     #`(let ([recv-label (recv SEND-PROC 'void)])
         (cond
           #,@(for/list ([label (syntax->list #'(LABEL ...))]
                         [expr (syntax->list #'(EXPR ...))])
                #`[(equal? recv-label #,label)
                   #,(expand-branches
                      expr)])))]
    [(E ...)
     #`(#,@(for/list ([expr (syntax->list #'(E ...))])
             (expand-branches expr)))]
    [_
     stx]))

(define-syntax (expand-process stx)
  (syntax-parse stx
    [(_ EXPR)
     (expand-branches
      (local-expand
       #'EXPR
       'expression
       '()))]))

(define-syntax (chor stx)
  (syntax-parse stx
    [(_ (PROC ...) GEXPR ...)
     (let* ([GEXPRS^
             (for/list ([proc (syntax->list #'(PROC ...))])
               #`(syntax-parameterize ([cur-process #'#,proc])
                   (define-process
                     #,proc
                     (expand-process
                      (block
                       (syntax-parameterize ([in-global-expr #t])
                         GEXPR ...))))
                   #f))])
       #`(define-network
           #,@GEXPRS^))]))

