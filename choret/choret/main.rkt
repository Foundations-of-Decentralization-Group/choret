#lang racket/base

(require "define-chor-syntax.rkt"
         "threads-network.rkt"
         racket/stxparam
         racket/block
         (for-syntax racket/base
                     racket/syntax
                     racket/hash
                     syntax/parse
                     (for-syntax racket/base)))

(provide chor)

;; The entry-point macro into Choret.
(define-syntax (chor stx)
  (syntax-parse stx
    [(_ (PROC ...) GEXPR ...)
     #'(define-network
         (syntax-parameterize ([cur-process #'PROC])
           (define-process PROC
             (expand-process
              (block
               (syntax-parameterize ([in-global-expr #t])
                 GEXPR ...))))) ...)]))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Syntax classes.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(begin-for-syntax
  ;; Syntax class for at forms used in binding positions. Ensures that a form
  ;; is only used in an "in-global-expr" context.
  (define-syntax-class at-id
    [pattern ((~datum at) PROCESS:id ID:id)
      #:fail-unless
      (syntax-parameter-value #'in-global-expr)
      "Cannot use an `at` identifier in a local expression!"
      #:attr process #'PROCESS
      #:attr id #'ID])

  ;; Syntax class for at forms which evaluate a local expression. Note that the
  ;; "lexpr" atribute has the LEXPR form wrapped with a syntax parameterization
  ;; that sets "in-global-expr" to #f. Also ensures that a form is only used in
  ;; an "in-global-expr" context.
  (define-syntax-class at-expr
    [pattern ((~datum at) PROCESS:id LEXPR:expr)
      #:fail-unless
      (syntax-parameter-value #'in-global-expr)
      "Cannot use an `at` identifier in a local expression!"
      #:attr process #'PROCESS
      #:attr lexpr #'(syntax-parameterize ([in-global-expr #f]) LEXPR)])

  ;; Syntax class for "branch?" forms which are hidden inside of "quote-syntax"
  ;; forms and which are tagged with the "'branch" syntax property.
  (define-syntax-class quoted-branch
    [pattern ((~literal quote-syntax)
              ((~literal branch?) SEND-PROC [LABEL GEXPR] ...)
              #:local)
      #:fail-unless
      (syntax-property this-syntax 'branch)
      "Expected a `branch?` form!"
      #:attr branch #'(branch? SEND-PROC [LABEL GEXPR] ...)]))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Definitions for new Choret syntax that does NOT override (or does not intend
;;; to override) existing Racket forms. These forms are only valid inside the
;;; body of a "chor" form.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(define/provide-chor-syntax (at stx)
  (syntax-parse stx
    [(_ PROC LEXPR)
     (if (cur-process? #'PROC)
         #'(syntax-parameterize ([in-global-expr #f])
             LEXPR)
         #'(void))]))

(define/provide-chor-syntax (~> stx)
  (syntax-parse stx
    [(_ SEND-AT:at-expr RECV-PROC)
     (cond [(cur-process? #'SEND-AT.PROCESS)
            #'(send RECV-PROC 'void SEND-AT.lexpr)]
           [(cur-process? #'RECV-PROC)
            #`(recv SEND-AT.PROCESS 'void)]
           [else #'(void)])]))

(define/provide-chor-syntax (sel~> stx)
  (syntax-parse stx
    [(_ SEND-PROC [RECV-PROC LABEL GEXPR] ...)
     (if (cur-process? #'SEND-PROC)
         #'(begin (choose! RECV-PROC LABEL GEXPR) ...)
         #`(begin
             #,@(for/list ([recv-proc (syntax->list #'(RECV-PROC ...))]
                           [gexpr (syntax->list #'(GEXPR ...))]
                           [label (syntax->list #'(LABEL ...))])
                  (if (eq-process? #'SEND-PROC recv-proc)
                      (raise-syntax-error
                       #f
                       "Process cannot send a selection to itself!"
                       #'SEND-PROC
                       recv-proc)
                      (if (cur-process? recv-proc)
                          #`(branch? SEND-PROC [#,label #,gexpr])
                          gexpr)))))]))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Definitions for new Choret syntax that DOES override existing Racket forms.
;;; These forms provide Choret specific functionality inside the body of a
;;; "chor" form, and outside of a "chor" form have their regular Racket
;;; functionality.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(define/provide-chor-syntax (let/chor stx) #:override let
  (syntax-parse stx
    [(_ ([AT-EXPR VAL-GEXPR] ...) GBODY ...)
     (let ([bindings
            (filter
             (lambda (x) x)
             (for/list ([expr-at (syntax->list #'(AT-EXPR ...))]
                        [val-expr (syntax->list #'(VAL-GEXPR ...))])
               (syntax-parse expr-at
                 [AT:at-id
                  (if (cur-process? #'AT.process)
                      #`[AT.id #,val-expr]
                      #`[#,(gensym 'IGNORE-) #,val-expr])]
                 [ID #`(ID #,val-expr)])))])
       #`(let #,bindings GBODY ...))]))

(define/provide-chor-syntax (define/chor stx) #:override define
  (syntax-parse stx
    [(_ AT:at-id BODY-GEXPR ...)
     (if (cur-process? #'AT.process)
         #'(define AT.id BODY-GEXPR ...)
         #'(let () BODY-GEXPR ...))]
    [(_ ID GEXPR)
     #'(define ID GEXPR)]))

(define/provide-chor-syntax (set!/chor stx) #:override set!
  (syntax-parse stx
    [(_ AT:at-id GEXPR)
     (if (cur-process? #'AT.process)
         #'(set! AT.id GEXPR)
         #'(let () GEXPR))]
    [(_ ID GEXPR)
     #'(set! ID GEXPR)]))

(define/provide-chor-syntax (lambda/chor stx) #:override lambda
  (syntax-parse stx
    [(_ (GARG ...) GBODY ...)
     (let ([args
            (for/list ([garg (syntax->list #'(GARG ...))])
              (syntax-parse garg
                [AT:at-id
                 (if (cur-process? #'AT.process)
                     #'AT.id
                     (format-id #'AT.id "UNUSED-located-at-~a" #'PROC))]
                [ARG #'ARG]))])
       #`(lambda #,args
           GBODY ...))]))

(define/provide-chor-syntax (if/chor stx) #:override if
  (syntax-parse stx
    [(_ AT:at-expr TRUE-GEXPR FALSE-GEXPR)
     (if (cur-process? #'AT.process)
         #'(if AT.lexpr TRUE-GEXPR FALSE-GEXPR)
         (merge
          (local-expand-expr #'TRUE-GEXPR)
          (local-expand-expr #'FALSE-GEXPR)))]
    [(_ REST ...)
     #'(if REST ...)]))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Definitions for Choret macros which are currently only used internally.
;;; These "helper" macros are often used to either help modularize the code or
;;; to force particular expansion/evaluation of syntax objects by the expander.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;; (choose! RECV-PROC LABEL GEXPR)
;; From the current process, sends a value LABEL to the process RECV-PROC which
;; tells the process RECV-PROC which branch to take. This is used only by the
;; "sel~>" macro to help implement knowledge of choice in choreographies.
(define-syntax (choose! stx)
  (syntax-parse stx
    [(_ RECV-PROC LABEL GEXPR)
     #'(begin
         (send RECV-PROC 'void LABEL)
         GEXPR)]))

;; (branch? SEND-PROC [LABEL GEXPR] ...)
;; Conceptually the inverse of the "choose!" macro. However, the branch cannot
;; be immediatly expanded into a "cond" form since the merging algorithm, when
;; merging the code of two branches, needs to inspect the LABELs of the branches
;; and recursively merge the GEXPRs of matching LABELs. Each of the individual
;; GEXPRs are expanded using local-expand, but branches are not expanded into
;; "cond" forms unitl after the projection/expansion for a given process has
;; finished. The expansion of "branch?" forms into "cond" forms is done by the
;; "expand-branches" function. Also, to simulate the functionality of Racket
;; core forms, a hack where "branch?" forms are wrapped inside of
;; "(quote-syntax ___ #:local)" forms is performed.
(define-syntax (branch? stx)
  (syntax-parse stx #:literals (branch?)
    [(branch? SEND-PROC [LABEL GEXPR] ...)
     (quote-syntax-branch
      #`(branch?
         SEND-PROC
         #,@(for/list ([label (syntax->list #'(LABEL ...))]
                       [gexpr (syntax->list #'(GEXPR ...))])
              #`[#,label
                 #,(local-expand-expr gexpr)])))]))

;; (expand-process EXPR)
;; This macro is needed by the "chor" macro to force Racket's expander to expand
;; the "define-process" macro (from threads-network.rkt) before applying
;; "local-expand" and "expand-branches" to EXPR. Expanding "define-process"
;; first is necessary for how threads-network.rkt is implemented, but this could
;; be improved in the future.
(define-syntax (expand-process stx)
  (syntax-parse stx
    [(_ EXPR) (expand-branches (local-expand-expr #'EXPR))]))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Phase-1 functions for performing merging and handling "branch?" forms.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;; Given two syntax objects, [left-branch] and [right-branch], performs merging
;; (in the choreographic sense) and if successful returns a single, merged,
;; syntax object. If merging fails a syntax error is raised.
(define-for-syntax (merge left-stx right-stx)
  (syntax-parse #`(#,left-stx #,right-stx)
    [(LEFT:quoted-branch RIGHT:quoted-branch)
     (merge-branches #'LEFT.branch #'RIGHT.branch)]
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

;; A helper function for "merge". Given two syntax objects, [left-branch] and
;; [right-branch], which are "branch?" forms, performs merging (in the
;; choreographic sense) and if successful returns a single syntax object for the
;; merged "branch?" form. If merging fails a syntax error is raised.
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
     (quote-syntax-branch
      #`(branch?
         L-SEND-PROC
         #,@
         (for/list ([pair (hash->list label-union)])
           (let ([label (car pair)]
                 [expr (cdr pair)])
             (if (pair? expr)
                 ;; The case where a label is in both branches being merged
                 #`[#,label #,(merge (car expr) (cdr expr))]
                 ;; The case where a label is in only one branch
                 #`[#,label #,expr])))))]))

;; After the expansions for projection and merging are complete for a process,
;; only Racket core forms are left. Some of these core forms are not truly fully
;; expanded though, there are "branch?" forms stealthily hidden away inside of
;; "(quote-syntax ___ #:local)" forms that still need to be converted to "cond"
;; forms. This function takes a syntax object of a projected process and
;; recursively checks for such hidden "branch?" forms and converts them into
;; "cond" forms.
(define-for-syntax (expand-branches stx)
  (syntax-parse (syntax-disarm stx #f)
    [BRANCH:quoted-branch
     (syntax-rearm
      #`(let ([recv-label (recv BRANCH.SEND-PROC 'void)])
          (cond
            #,@(for/list ([label (syntax->list #'(BRANCH.LABEL ...))]
                          [expr (syntax->list #'(BRANCH.GEXPR ...))])
                 #`[(equal? recv-label #,label)
                    #,(expand-branches
                       expr)])))
      stx)]
    [(E ...)
     #`(#,@(for/list ([expr (syntax->list #'(E ...))])
             (expand-branches expr)))]
    [_
     stx]))


;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;;; Miscellaneous phase-1 helper functions and definitions.
;;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


;; Contains the name of the process which is currently being projected/expanded.
(define-syntax-parameter cur-process
  (lambda (stx)
    (raise-syntax-error
     #f
     "Use of `cur-process` outside of a projection!"
     stx)))

;; Checks if [proc], which must be a syntax object, refer to the same process
;; as the process identifier held by the [cur-process] syntax parameter. This
;; is useful when projecting syntax that must be projected differently
;; depending on what process is currently being projected.
(define-for-syntax (cur-process? proc)
  (free-identifier=? (syntax-parameter-value #'cur-process) proc))

;; Checks if [proc1] and [proc2], which must both be syntax objects, refer to
;; the same process.
(define-for-syntax (eq-process? proc1 proc2)
  (free-identifier=? proc1 proc2))

;; Convenience function for local-expand.
(define-for-syntax (local-expand-expr stx)
  (local-expand stx 'expression '()))

;; Returns a syntax object which uses "quote-syntax" to hide [stx] from
;; expansion and adds the "'branch" syntax property to help distinguish it from
;; possible regular uses of "quote-syntax".
(define-for-syntax (quote-syntax-branch stx)
  (syntax-property #`(quote-syntax #,stx #:local) 'branch #t))
