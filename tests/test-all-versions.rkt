#lang racket/base

(require racket/port)

(define (run-command
         #:output [output 'on-fail]
         cmd . args )
  (printf
   "$> ~a~n"
   (foldl (lambda (str arg) (string-append arg " " str)) cmd args))
  (define-values (sp out in err) (apply subprocess #f #f #f cmd args))
  (subprocess-wait sp)
  (define success? (equal? 0 (subprocess-status sp)))
  (printf "~a~n" (if success? "Success" "Failure"))
  (when (or
         (and (equal? output 'on-fail) (not success?))
         (equal? output 'always))
    (printf "stdout:\n~a" (port->string out))
    (printf "stderr:\n~a" (port->string err)))
  (close-input-port out)
  (close-output-port in)
  (close-input-port err)
  (subprocess-status sp))

(define (raco-cross
         #:output [output 'on-fail]
         version vm . args)
  (let ([version-str (format "~a" version)]
        [vm-str (format "~a" vm)])
    (apply run-command
           #:output output
           "/bin/raco" "cross"
           "--version" version-str
           "--vm" vm-str
           args)))

(define-syntax-rule (and-commands CMD ...)
  (if (and (equal? 0 CMD) ...) 0 1))

(define (run-tests version vm)
  (raco-cross
   #:output #f
   version vm "pkg" "install" "--batch" "--auto" "--copy" "../choret")
  (define result
    (and-commands
     (raco-cross version vm "make" "test-all.rkt")
     (raco-cross version vm "setup" "-l" "choret")
     (raco-cross version vm "test" "-e" "-m" "test-all.rkt")))
  (printf
   "Test for version=~a, vm=~a: ~a~n"
   version
   vm
   (if (equal? result 0) "PASSED" "FAILED")))

(run-tests 8.14 'cs)
(run-tests 8.14 'bc)
(run-tests 8.2 'cs)
(run-tests 8.2 'bc)
(run-tests 8.0 'cs)
(run-tests 8.0 'bc)
(run-tests 7.9 'cs)
(run-tests 7.9 'bc)
(run-tests 7.4 'cs)
(run-tests 7.4 'bc)
(run-tests 7.0 'bc)
(run-tests 6.12 'bc)
(run-tests 6.6 'bc)
