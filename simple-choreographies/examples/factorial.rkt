#lang s-exp "../simple-choreographies.rkt"

(require racket/stream rackunit)

(define-syntax-rule (compute-local-factorial result range)
  (set! result (foldl * 1 (stream->list (in-range (car range) (cdr range))))))

(define-syntax define-local-multiple
  (chor-macro
   (lambda (stx)
     (syntax-case stx ()
       [(_ (proc ...) name value)
        #'(begin/chor (define-local proc name value) ...)]))))

(define-syntax gather->
  (chor-macro
   (lambda (stx)
     (syntax-case stx ()
       [(_ [(senders ...) expr] [reciever (ids ...)])
        (let ([senders* (syntax->list #'(senders ...))]
              [ids* (syntax->list #'(ids ...))])
          #`(begin/chor
             #,@(map
                 (lambda (sender id) #`(com-> [#,sender expr] [reciever #,id]))
                 senders*
                 ids*)))]))))

(define-chor [Main P1 P2 P3 P4 P5 P6 P7 P8]
  ; Split the work of coputing the factorial into ranges for each
  ; process.
  (define-local Main factorial 120)
  (define-local Main num-processes 8)
  (define-local Main step (ceiling (/ factorial num-processes)))
  (define-local
   Main
   ranges (build-vector
           num-processes
           (lambda (x)
             (let ([start (add1 (* step x))]
                   [end (add1 (* step (add1 x)))])
               (cons start (min end (add1 factorial)))))))
  (define-local Main final-result 0)

  ; Send the ranges to all the processes
  ; begin/chor forms were added just to test/demonstrate that the begin/chor
  ; form works
  (com-> [Main (vector-ref ranges 0)] [P1 range])
  (com-> [Main (vector-ref ranges 1)] [P2 range])
  (com-> [Main (vector-ref ranges 2)] [P3 range])
  (com-> [Main (vector-ref ranges 3)] [P4 range])
  (com-> [Main (vector-ref ranges 4)] [P5 range])
  (com-> [Main (vector-ref ranges 5)] [P6 range])
  (com-> [Main (vector-ref ranges 6)] [P7 range])
  (com-> [Main (vector-ref ranges 7)] [P8 range])

  (define-local-multiple (P1 P2 P3 P4 P5 P6 P7 P8) result 0)

  (define-syntax/chor print-local-result
    (lambda (stx)
      (syntax-case stx ()
        [(_ proc-name)
         #'(expr-local
            proc-name
            (printf "Local result on ~a: ~a\n" 'proc-name result))])))

  (define-syntax/chor comp-locals
    (lambda (stx)
      (syntax-case stx ()
        [(_ proc-name ...)
         #`(begin/chor
            #,@(for/list ([i (syntax->list #'(proc-name ...))])
                 #`(begin/chor
                    (expr-local #,i (compute-local-factorial result range))
                    (print-local-result #,i))))])))

  ; Have each process compute its local value
  (comp-locals P1 P2 P3 P4 P5 P6 P7 P8)

  ; Send the result of computing each range product back to the Main process
  (gather->
   [(P1 P2 P3 P4 P5 P6 P7 P8) result]
   [Main (p1-res p2-res p3-res p4-res p5-res p6-res p7-res p8-res)])

  ; Let the Main process compute the overall product to calculate the factorial
  (expr-local Main
              (set! final-result (* p1-res p2-res p3-res p4-res
                                    p5-res p6-res p7-res p8-res))
   (check-equal?
    final-result
    6689502913449127057588118054090372586752746333138029810295671352301633557244962989366874165271984981308157637893214090552534408589408121859898481114389650005964960521256960000000000000000000000000000)
   (printf "120! = ~a\n" final-result)))

