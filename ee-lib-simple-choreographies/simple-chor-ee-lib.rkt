#lang racket

(require "../simple-networks/simple-networks.rkt")
(require ee-lib/define)
(require (for-syntax ee-lib racket/base (for-syntax racket/base)))

(provide require)
(provide #%app #%datum #%top-interaction #%top #%module-begin)

(provide define-chor)

;; (define-literal-forms chor-literals ""
;;   (local-expr com->))

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

  (struct choret-macro [transformer])
  (struct chor-process-variable [])

  (provide choret-macro)

  (define (valid-processes? . processes)
    (for/and ([proc processes])
      (unless (lookup proc)
        (raise-syntax-error #f "Undefined process name!" proc))))

  ;; For the given syntax of the choreography [stx] and a process name
  ;; [process-name], project the program specifically for the given
  ;; [process-name] and wrap it in a 'define-process' form.
  (define (project-process stx process-name)
    (with-scope sc
      #`(define-process #,process-name
          #,@(project-chor-exprs stx process-name)
          (printf "End of process ~a\n" '#,process-name))))

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
    (syntax-case stx
      (local-define local-expr com-> chor-begin define-syntax/chor)
      [(local-define local-proc id local-expression)
       (valid-processes? #'local-proc)
       (cond-proc process-name
                  [#'local-proc
                   (with-syntax ([id^
                                  (bind! #'id (racket-var))]
                                 [local-expr^
                                  (local-expand
                                   #'local-expression
                                   (list (current-ctx-id))
                                   '()
                                   (list (current-def-ctx)))])
                     #'(define id^ local-expr^))])]
      [(local-expr local-proc local-proc-exprs ...)
       (valid-processes? #'local-proc)
       (cond-proc process-name
                  [#'local-proc
                   (with-syntax ([local-proc-exprs^
                                  (local-expand
                                   #'(begin local-proc-exprs ...)
                                   (list (current-ctx-id))
                                   '()
                                   (list (current-def-ctx)))])
                     #'(begin local-proc-exprs^))])]
      [(com-> [sender sender-local-expr] [reciever reciever-local-var])
       (valid-processes? #'sender #'reciever)
       (cond-proc process-name
                  [#'sender
                   (with-syntax ([sender-local-expr^
                                  (local-expand
                                   #'sender-local-expr
                                   (list (current-ctx-id))
                                   '()
                                   (list (current-def-ctx)))])
                     #'(send reciever 'any sender-local-expr^))]
                  [#'reciever
                   (with-syntax ([reciever-local-var^
                                  (bind! #'reciever-local-var (racket-var))])
                     #'(define reciever-local-var^ (recv sender 'any)))])]
      [(chor-begin body ...)
       (let* ([stx^ (project-chor-exprs #'(body ...) process-name)])
         (if (eq? (syntax-e stx^) '())
             #f
             #`(begin #,@(syntax->list stx^))))]
      [(define-syntax/chor name val)
       (bind! #'name (choret-macro (eval-transformer #'val)))
       #f]
      [(macro-name body ...)
       (lookup #'macro-name choret-macro?)
       (let* ([transformer
               (choret-macro-transformer
                (lookup #'macro-name choret-macro?))]
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
             #`(define-network #,@proc-list^)))])))
