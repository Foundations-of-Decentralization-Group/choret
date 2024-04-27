#lang racket

(require "../simple-networks/simple-networks.rkt")
(require (for-syntax racket))

(provide require)
(provide #%app #%datum #%top-interaction #%top #%module-begin)

(provide define-chor)

(begin-for-syntax
  ;; A "macro-macro" meant to be used in the "project-process-expr" syntax
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
                     #`[(equal?
                         process-name
                         (syntax->datum case-name))
                        case-body]))
                 (syntax->list #'(cases ...)))
             [else #f]))])))

;; A syntax function that performs process projection for a single expression.
;; Technically speking this function is not a macro transformer.
;;
;; [process-name] is a syntax object which should contain a symbol datum which
;; represents the process to project on.
;;
;; [stx] is the syntax object for which to do the projection. It is pattern-
;; matched against the possible forms for the simple-choreographies language,
;; which currently includes the [com->] and [local-expr] forms.
(define-for-syntax (project-process-expr process-name stx)
  (syntax-case stx (com-> local-expr)
    [(com-> [sender sender-local-expr] [reciever reciever-local-var])
       (cond-proc process-name
                  [#'sender
                   #'(send reciever 'any sender-local-expr)]
                  [#'reciever
                   #'(define reciever-local-var (recv sender 'any))])]
    [(local-expr local-proc local-proc-exprs ...)
     (cond-proc process-name
                [#'local-proc
                 #'(begin local-proc-exprs ...)])]
    [(select-> sender reciever label)
     (raise-syntax-error
      #f
      "Not implemented!"
      stx)]
    [(if-> condition [cases] ...)
     (raise-syntax-error
      #f
      "Not implemented!"
      stx)]
    [_
     (raise-syntax-error
      #f
      "Not a valid simple-choreographies s-expression!"
      stx)]))

;; A syntax function that performs process projection for a sequence of
;; expressions. Technically speking this function is not a macro transformer.
;;
;; [process-name] is a syntax object which should contain a symbol datum which
;; represents the process to project on.
;;
;; [stx] is the syntax object for which to do the projection. It should be an
;; expression containing a sequence of nested sub-expressions which each will
;; be projected for the given process.
(define-for-syntax (project-process process-name stx)
  (syntax-case stx ()
    [(process-expr process-exprs ...)
     (cons
      (project-process-expr process-name #'process-expr)
      (project-process process-name #'(process-exprs ...)))]
    [() '()]))

;; A macro transformer which defines and creates a projection for a
;; choreography.
;;
;; It has the following form:
;; (define-chor [PROC-NAMES ...] EXPRS ...)
;;
;; [PROC-NAMES ...] is a list of process names which exist in the choreography.
;; Only the processes mentioned in this list will be projected. (The
;; implementation currently only uses this to make it easier to determine which
;; processes need to be projected.)
;;
;; [EXPRS] is a list of expressions, each expression being an expression of
;; simple-choreographies; this is what actually describes the choreography.
(define-syntax (define-chor stx)
  (syntax-case stx ()
    [(define-chor (proc-names ...) chor-stx ...)
     ; [expr-list] is a list of syntax objects, where each syntax object is the
     ; projection for one of the processes from [PROC-NAMES].
     (let* ([expr-list
             (map (lambda (proc-name)
                    #`(define-process #,proc-name
                        #,@(project-process proc-name #'(chor-stx ...))
                        (printf "End of process ~a\n" '#,proc-name)))
                  (syntax->list #'(proc-names ...)))]
            )
       #`(define-network #,@expr-list (printf "Start chor:\n")))]))

