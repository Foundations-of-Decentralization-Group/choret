#lang racket/base

(require
 racket/base ee-lib/define
 (except-in "../simple-projections/simple-projections.rkt" #%module-begin)
 (for-syntax
  racket/base ee-lib syntax/parse
  (for-syntax
   racket/base)))

(provide
 (all-from-out racket/base)
 ;; Provide the interface macro for Choret
 define-chor
 ;; Provide the literals of Choret
 define-local expr-local com-> begin/chor define-syntax/chor
 (for-syntax
  (all-from-out racket/base)
  ;; Provide the struct representing a Choret macro.
  chor-macro))

(define-literal-forms chor-literals "Cannot use a Choret literal in Racket!"
  (define-local expr-local com-> begin/chor define-syntax/chor))

(begin-for-syntax

  ;; A "macro-macro" meant to be used in the "project-chor-expr" syntax
  ;; function.
  ;;
  ;; [cond-proc] has the following form:
  ;; (cond-proc PROCESS-NAME CASES ...)
  ;;
  ;; PROCESS-NAME must be a variable containing syntax object; this syntax oject
  ;; should simply conain a symbol datum representing the name of the process to
  ;; project onto.
  ;;
  ;; CASES describes what to do for a given match against the process name;
  ;; it has the following form:
  ;; (CASE-NAME CASE-EXPR)
  ;;
  ;; CASE-NAME must be a variable containing syntax object; this syntax oject
  ;; should simply conain a symbol datum representing the name of the process to
  ;; match against PROCESS-NAME.
  ;;
  ;; CASE-EXPR is the expression to evaluate if CASE-NAME and PROCESS-NAME
  ;; have equal symbol datums.
  ;;
  ;; In the event that none of the CASE-NAMEs match PROCESS-NAME, then #f is
  ;; returned.
  (define-syntax (cond-proc stx)
    (syntax-case stx ()
      [(cond-proc proc-name cases ...)
       #`(let ([process-name (syntax->datum proc-name)])
           (cond
             #,@(map
                 (lambda (case-expr)
                   (with-syntax ([(case-name case-body) case-expr])
                     #`[(free-identifier=? case-name proc-name)
                        case-body]))
                 (syntax->list #'(cases ...)))
             [else #f]))]))

  (struct chor-macro [transformer])
  (struct chor-process-variable [])

  (define (valid-processes? . processes)
    (for/and ([proc processes])
      (unless (lookup proc)
        (raise-syntax-error #f "Undefined process name!" proc))))

  ;; For the given syntax of the choreography [stx] and a process name
  ;; [process-name], project the program specifically for the given
  ;; [process-name] and wrap it in a 'define-process' form.
  (define (project-process stx process-name)
    (with-scope sc
      #`(define-projection #,process-name
          #,@(project-chor-exprs (add-scope stx sc) process-name)
          (expr-local/proj (printf "End of process ~a\n" '#,process-name)))))

  ;; For the given body syntax of the choreography [stx] and a process name
  ;; [process-name], recursively get the next program term/expression and
  ;; perform projection for that individual term.
  (define (project-chor-exprs stx process-name)
    (syntax-case stx ()
      [(chor-expr chor-exprs ...)
       (with-syntax ([chor-expr^
                      (project-chor-expr #'chor-expr process-name)]
                     [chor-exprs^
                      (project-chor-exprs #'(chor-exprs ...) process-name)])
         (if (syntax->datum #'chor-expr^)
             #`(chor-expr^ (~@ . chor-exprs^))
             #'chor-exprs^))]
      [() #'()]))

  ;; Project an individual program term/expression.
  (define (project-chor-expr stx process-name)
    (syntax-parse stx #:literal-sets (chor-literals)
      [(define-local local-proc id local-expression)
       #:when (valid-processes? #'local-proc)
       (cond-proc process-name
                  [#'local-proc
                   #'(define/proj id local-expression)])]


      [(expr-local local-proc local-proc-exprs ...)
       #:when (valid-processes? #'local-proc)
       (cond-proc process-name
                  [#'local-proc
                   #'(expr-local/proj local-proc-exprs ...)])]


      [(com-> [sender sender-local-expr] [reciever reciever-local-var])
       #:when (valid-processes? #'sender #'reciever)
       (cond-proc process-name
                  [#'sender
                   #'(send!/proj reciever sender-local-expr)]
                  [#'reciever
                   #`(recv?/proj sender reciever-local-var)])]


      [(begin/chor body ...)
       (let* ([stx^ (project-chor-exprs #'(body ...) process-name)])
         (if (eq? (syntax-e stx^) '())
             #f
             #`(begin/proj #,@(syntax->list stx^))))]


      [(define-syntax/chor name val)
       (bind! #'name (chor-macro (eval-transformer #'val)))
       #f]


      [(macro-name body ...)
       #:when (lookup #'macro-name chor-macro?)
       (let* ([transformer
               (chor-macro-transformer
                (lookup #'macro-name chor-macro?))]
              [stx^ (transformer stx)]
              [stx^^ (project-chor-expr stx^ process-name)])
         (if stx^^ #`(#,@(syntax->list stx^^)) #f))])))

;; Entry point macro for simple-choreographies; calls the 'project-process'
;; syntax transformer, which implements the simple-choreographies DSL using the
;; 'ee-lib' library.
(define-syntax (define-chor stx)
  (with-scope chor-top-sc
      (syntax-case stx ()
        [(_ (proc-names ...) chor-exprs ...)
         (with-syntax ([chor-exprs^ (add-scope #'(chor-exprs ...) chor-top-sc)])
           (let* ([proc-bindings
                   (map (lambda (proc-name)
                          (bind!
                           (add-scope proc-name chor-top-sc)
                           (chor-process-variable)))
                        (syntax->list #'(proc-names ...)))]
                  [proc-list^
                   (map (lambda (proc-name)
                          (project-process #'chor-exprs^ proc-name))
                        proc-bindings)])
             #`(simple-projections #,@proc-list^)))])))
