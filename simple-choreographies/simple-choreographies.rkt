#lang racket

(require "../simple-networks/simple-networks.rkt")
(require (for-syntax racket))

(provide require)
(provide #%app #%datum #%top-interaction #%top #%module-begin)

(provide define-chor)

(begin-for-syntax
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

(define-for-syntax (project-process process-name stx)
  (syntax-case stx ()
    [(process-expr process-exprs ...)
     (cons
      (project-process-expr process-name #'process-expr)
      (project-process process-name #'(process-exprs ...)))]
    [() '()]))

(define-syntax (define-chor stx)
  (syntax-case stx ()
    [(define-chor (proc-names ...) chor-stx ...)
     (let* ([expr-list
             (map (lambda (proc-name)
                    #`(define-process #,proc-name
                        #,@(project-process proc-name #'(chor-stx ...))
                        (printf "End of process ~a\n" '#,proc-name)))
                  (syntax->list #'(proc-names ...)))]
            )
       #`(define-network #,@expr-list (printf "Start chor:\n")))]))

