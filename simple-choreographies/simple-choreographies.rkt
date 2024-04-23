#lang racket

(require "../simple-networks/simple-networks.rkt")

(provide require)
(provide #%app #%datum #%top-interaction #%top #%module-begin)

(provide define-chor)

(define-for-syntax (project-process-expr process-name stx)
  (syntax-case stx (com->)
    [(com-> [sender sender-local-expr] [reciever reciever-local-var])
     (let ([sender-name (syntax->datum #'sender)]
           [reciever-name (syntax->datum #'reciever)]
           [process-name (syntax->datum process-name)])
       (if (equal? process-name sender-name)
           #'(send reciever 'any sender-local-expr)
           (if (equal? process-name reciever-name)
               #'(define reciever-local-var (recv sender 'any))
               #f)))]
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
                  (remove #f (syntax->list #'(proc-names ...))))]
            )
       #`(define-network #,@expr-list (printf "Start chor:\n")))]))

