#lang racket

(require racket/stxparam)

(require racket/base)
(provide require)

(provide define-network define-process send recv)
(provide #%app #%datum #%top-interaction #%top)

(struct process-data (name thread) #:prefab)
(begin-for-syntax
  (define channel-count 0)
  (define channels (make-hash)))

;; Keeps track of the "init-processes" variable introduced in the
;; "define-network" macro. The variable accumulates, at runtime, a list of
;; channels that are used to initialize each simple-process.
(define-syntax-parameter network-init-channels-stxparam
  (lambda (stx)
    (raise-syntax-error
     #f
     "Cannot use define-proceess outside of a network"
     stx)))

;; Keeps track of the name of the current process introduced by the
;; "define-process" macro.
(define-syntax-parameter current-process-name-stxparam
  (lambda (stx)
    (raise-syntax-error
     #f
     "Cannot use send or recv outside of a process"
     stx)))

;; Keeps track of the "channel-vector" variable introduced by the
;; "define-process" macro. This variable is initialized at runtime with a
;; vector which indexes the list of channels which processes use to
;; communicate with each other. Each index corresponds to a unique process
;; pair.
(define-syntax-parameter current-process-channel-vector-stxparam
  (lambda (stx)
    (raise-syntax-error
     #f
     "Cannot use send or recv outside of a process"
     stx)))

;; Given a pair of process names (each name provided as a symbol), if a
;; unique index has already been assigned for that pair, return the index for
;; that pair, otherwise assign a new index for that pair and return the new
;; index.
(define-for-syntax (find-or-create-channel-index proc-x proc-y)
  ; Sort the pair so that the order of the process names does not matter.
  (let* ([procs (sort (map symbol->string (list proc-x proc-y)) string<?)])
    (or
     ; Check if the given channel pair already has an index assigned to it; if
     ; it does, return that index.
     (hash-ref channels procs #f)
     ; Otherwise generate, record, and return a new index for the given channel
     ; pair.
     (begin
       (printf "new pair ~a assigned index ~a\n" procs channel-count)
       (hash-set! channels procs channel-count)
       (set! channel-count (+ 1 channel-count))
       (- channel-count 1)))))

(define-syntax (recv stx)
  (syntax-case stx ()
    [(recv target-process type)
     (with-syntax
         ;; Find the index for the given process pair for target-process and
         ;; current-process.
         ([channel-index
           (find-or-create-channel-index
            ((syntax-parameter-value #'current-process-name-stxparam) #f)
            (syntax->datum #'target-process))]
          ;; Get the channel vector for this process to look up the channel for
          ;; the given channel-index.
          [channel-vector
           ((syntax-parameter-value
             #'current-process-channel-vector-stxparam)
            #f)])
       #'(begin
           (printf "recv on index ~a\n" channel-index)
           (channel-get (vector-ref channel-vector channel-index))))]))

(define-syntax (send stx)
  (syntax-case stx ()
    [(recv target-process type data)
     (with-syntax
         ;; Find the index for the given process pair for target-process and
         ;; current-process.
         ([channel-index
           (find-or-create-channel-index
            ((syntax-parameter-value #'current-process-name-stxparam) #f)
            (syntax->datum #'target-process))]
          ;; Get the channel vector for this process to look up the channel for
          ;; the given channel-index.
          [channel-vector
           ((syntax-parameter-value
             #'current-process-channel-vector-stxparam)
            #f)])
       #'(begin
           (printf "send on index ~a\n" channel-index)
           (channel-put (vector-ref channel-vector channel-index) data)))]))

(define-syntax (define-process stx)
  (syntax-case stx ()
    [(define-process process-name process-stx ...)
     (with-syntax
         ([init-channels
           (syntax-parameter-value #'network-init-channels-stxparam)])
       #'(begin
           ;; (At run time) Make a channel for this process's init-channel and
           ;; add it to the list of initialization channels (init-channels).
           (define init-channel (make-channel))
           (set! init-channels (cons init-channel init-channels))
           (define process-name
             (process-data
              'process-name
              (thread (lambda ()
                        (begin
                          (define channel-vector (channel-get init-channel))
                          (syntax-parameterize
                              ([current-process-name-stxparam
                                (lambda (stx) 'process-name)]
                               [current-process-channel-vector-stxparam
                                (lambda (stx) #'channel-vector)])
                            process-stx ...)
                          (channel-put init-channel 'process-name))))))))]))

(define-syntax (define-network stx)
  (syntax-case stx ()
    [(define-network network-stx ...)
     (with-syntax ([channel-count channel-count])
       #'(begin
           (define init-channels '())
           (syntax-parameterize
               ([network-init-channels-stxparam #'init-channels])
             network-stx ...
             (define channel-vector
               (build-vector
                100
                (lambda (_) (make-channel))))
             ;; Send each process the channel vector
             (for ([init-channel init-channels])
               (channel-put init-channel channel-vector))
             ;; Wait for each process to finish
             (for ([init-channel init-channels])
               (channel-get init-channel)))))]))

(define-syntax (simple-networks-module-begin stx)
  (syntax-case stx ()
    [(simple-networks-module-begin body ...)
       #'(#%module-begin body ...)]))

(provide (rename-out [simple-networks-module-begin #%module-begin]))
