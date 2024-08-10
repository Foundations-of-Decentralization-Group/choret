#lang racket

(require choret
         racket/stream
         rackunit
         (for-syntax racket/base
                     racket/syntax))

(define-syntax-rule (compute-local-factorial RESULT RANGE)
  (foldl * 1 (stream->list (in-range (car RANGE) (cdr RANGE)))))

(define-syntax-rule (~>/define* [SEND (at RECV RECV-ID)] ...)
  (begin (define (at RECV RECV-ID) (~> SEND RECV)) ...))

(define-syntax-rule (gather~> [(at SENDER LEXPR) ...] RECIEVER)
  (list (~> (at SENDER LEXPR) RECIEVER) ...))

(chor (main P1 P2 P3 P4 P5 P6 P7 P8)
      (define (at main factorial) (at main 120))
      (define (at main num-processes) (at main 8))
      (define (at main step) (at main (ceiling (/ factorial num-processes))))
      (define (at main ranges)
        (at main
            (build-vector
             num-processes
             (lambda (x)
               (let ([start (add1 (* step x))]
                     [end (add1 (* step (add1 x)))])
                 (cons start (min end (add1 factorial))))))))
      (define (at Main final-result) (at Main 1))
      (~>/define* [(at main (vector-ref ranges 0)) (at P1 range)]
                  [(at main (vector-ref ranges 1)) (at P2 range)]
                  [(at main (vector-ref ranges 2)) (at P3 range)]
                  [(at main (vector-ref ranges 3)) (at P4 range)]
                  [(at main (vector-ref ranges 4)) (at P5 range)]
                  [(at main (vector-ref ranges 5)) (at P6 range)]
                  [(at main (vector-ref ranges 6)) (at P7 range)]
                  [(at main (vector-ref ranges 7)) (at P8 range)])

      (define-syntax-rule (compute-locals RESULT [PROC-NAME ...])
        (begin
          (define (at PROC-NAME RESULT)
            (at PROC-NAME (compute-local-factorial result range))) ...
          (at PROC-NAME (println result)) ...))

      (compute-locals result [P1 P2 P3 P4 P5 P6 P7 P8])
      (at l3 (println 'test1))

      (define (at main results)
        (gather~> [(at P1 result)
                   (at P2 result)
                   (at P3 result)
                   (at P4 result)
                   (at P5 result)
                   (at P6 result)
                   (at P7 result)
                   (at P8 result)]
                  main))

      (define (at main final-result) (at main (foldl * 1 results)))
      (at l3 (println 'test4))

      (at main
          (begin
            (check-equal?
             final-result
             6689502913449127057588118054090372586752746333138029810295671352301633557244962989366874165271984981308157637893214090552534408589408121859898481114389650005964960521256960000000000000000000000000000)
            (printf "120! = ~a\n" final-result))))
