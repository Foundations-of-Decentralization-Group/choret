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
       #`(cond
           #,@(map
               (lambda (case-expr)
                 (with-syntax ([(case-name case-body) case-expr])
                   #`[(equal? proc-name case-name) case-body]))
               (syntax->list #'(cases ...)))
           [else #f])])))

(define-for-syntax (project-process-expr process-name stx)
  (syntax-case stx (com-> local-expr)
    [(com-> [sender sender-local-expr] [reciever reciever-local-var])
       (cond-proc (syntax->datum process-name)
                  [(syntax->datum #'sender)
                   #'(send reciever 'any sender-local-expr)]
                  [(syntax->datum #'reciever)
                   #'(define reciever-local-var (recv sender 'any))])]
    [(local-expr local-proc local-proc-exprs ...)
     (cond-proc (syntax->datum #'local-proc)
                [(syntax->datum process-name)
                 #'(begin local-proc-exprs ...)])]
    [(select-> sender reciever label)
       #'(println 'not-implemented)]
    [(if-> condition [cases] ...)
       #'(println 'not-implemented)]
    [()
     #'(println 'error-empty)]))

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

