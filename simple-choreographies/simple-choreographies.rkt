#lang s-exp "../simple-networks/simple-networks.rkt"

(require racket)

(require racket/stxparam)
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
           #'(printf "~a!~a; " 'sender 'sender-local-expr)
           (if (equal? process-name reciever-name)
               #'(printf "~a?~a; " 'reciever 'reciever-local-var)
               #'(println 'error))))]
    [(select-> sender reciever label)
       #'(println 'not-implemented)]
    [(if-> condition [cases] ...)
       #'(println 'not-implemented)]
    [()
     #'(println 'error)]))

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
     (let ([expr-lists
            (map (lambda (proc-name)
                   (cons
                    #`(printf "\n\nProjection: ~a\n  " '#,proc-name)
                    (project-process proc-name #'(chor-stx ...))))
             (syntax->list #'(proc-names ...)))])
       (let ([expr-list (foldl append '() expr-lists)])
         #`(begin #,@expr-list)))]))

