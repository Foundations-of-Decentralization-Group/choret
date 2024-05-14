#lang s-exp "../simple-choreographies.rkt"

(require racket/base (for-syntax racket/base))
(require racket/stream)
(require rackunit)

(define-syntax-rule (compute-local-factorial result range)
  (define result (foldl * 1 (stream->list (in-range (car range) (cdr range))))))

(define-chor [Main P1 P2 P3 P4 P5 P6 P7 P8]
  (local-expr Main
              ; Split the work of coputing the factorial into ranges for each
              ; process.
              (define factorial 120)
              (define num-processes 8)
              (define step (ceiling (/ factorial num-processes)))
              (define ranges (build-vector
                              num-processes
                              (lambda (x)
                                (let ([start (add1 (* step x))]
                                      [end (add1 (* step (add1 x)))])
                                  (cons start (min end (add1 factorial))))))))

  ; Send the ranges to all the processes
  (com-> [Main (vector-ref ranges 0)] [P1 range])
  (com-> [Main (vector-ref ranges 1)] [P2 range])
  (com-> [Main (vector-ref ranges 2)] [P3 range])
  (com-> [Main (vector-ref ranges 3)] [P4 range])
  (com-> [Main (vector-ref ranges 4)] [P5 range])
  (com-> [Main (vector-ref ranges 5)] [P6 range])
  (com-> [Main (vector-ref ranges 6)] [P7 range])
  (com-> [Main (vector-ref ranges 7)] [P8 range])

  ; Have each process compute its local value
  (local-expr P1 (compute-local-factorial result range))
  (local-expr P2 (compute-local-factorial result range))
  (local-expr P3 (compute-local-factorial result range))
  (local-expr P4 (compute-local-factorial result range))
  (local-expr P5 (compute-local-factorial result range))
  (local-expr P6 (compute-local-factorial result range))
  (local-expr P7 (compute-local-factorial result range))
  (local-expr P8 (compute-local-factorial result range))

  ; Send the result of computing each range product back to the Main process
  (com-> [P1 result] [Main p1-res])
  (com-> [P2 result] [Main p2-res])
  (com-> [P3 result] [Main p3-res])
  (com-> [P4 result] [Main p4-res])
  (com-> [P5 result] [Main p5-res])
  (com-> [P6 result] [Main p6-res])
  (com-> [P7 result] [Main p7-res])
  (com-> [P8 result] [Main p8-res])

  ; Let the Main process compute the overall product to calculate the factorial
  (local-expr Main
   (define final-result (* p1-res p2-res p3-res p4-res
                           p5-res p6-res p7-res p8-res))
   (check-equal?
    final-result
    6689502913449127057588118054090372586752746333138029810295671352301633557244962989366874165271984981308157637893214090552534408589408121859898481114389650005964960521256960000000000000000000000000000)
   (printf "120! = ~a\n" final-result))
  )

