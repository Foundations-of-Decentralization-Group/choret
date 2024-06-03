#lang s-exp "simple-chor-ee-lib.rkt"

(require racket/base (for-syntax racket/base))
(require racket/stream)
(require rackunit)

(define-syntax-rule (compute-local-factorial result range)
  (set! result (foldl * 1 (stream->list (in-range (car range) (cdr range))))))

;; (define-syntax comp-local
;;   (choret-macro
;;    (lambda (stx)
;;      (syntax-case stx ()
;;        [(_ proc-name)
;;         #'((local-define proc-name result 0)
;;            (local-expr proc-name (compute-local-factorial result range)))]))))

(define-syntax print-local-result
  (choret-macro
   (lambda (stx)
     (syntax-case stx ()
       [(_ proc-name)
        #'(local-expr
           proc-name
           (printf "Local result on ~a: ~a\n" 'proc-name result))]))))

(define-chor [Main P1 P2 P3 P4 P5 P6 P7 P8]
  ; Split the work of coputing the factorial into ranges for each
  ; process.
  (local-define Main factorial 120)
  (local-define Main num-processes 8)
  (local-define Main step (ceiling (/ factorial num-processes)))
  (local-define
   Main
   ranges (build-vector
           num-processes
           (lambda (x)
             (let ([start (add1 (* step x))]
                   [end (add1 (* step (add1 x)))])
               (cons start (min end (add1 factorial)))))))
  (local-define Main final-result 0)

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
  (local-define P1 result 0)
  (local-expr P1 (compute-local-factorial result range))
  (print-local-result P1)
  (local-define P2 result 0)
  (local-expr P2 (compute-local-factorial result range))
  (print-local-result P2)
  (local-define P3 result 0)
  (local-expr P3 (compute-local-factorial result range))
  (print-local-result P3)
  (local-define P4 result 0)
  (local-expr P4 (compute-local-factorial result range))
  (print-local-result P4)
  (local-define P5 result 0)
  (local-expr P5 (compute-local-factorial result range))
  (print-local-result P5)
  (local-define P6 result 0)
  (local-expr P6 (compute-local-factorial result range))
  (print-local-result P6)
  (local-define P7 result 0)
  (local-expr P7 (compute-local-factorial result range))
  (print-local-result P7)
  (local-define P8 result 0)
  (local-expr P8 (compute-local-factorial result range))
  (print-local-result P8)

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
              (set! final-result (* p1-res p2-res p3-res p4-res
                                    p5-res p6-res p7-res p8-res))
   (check-equal?
    final-result
    6689502913449127057588118054090372586752746333138029810295671352301633557244962989366874165271984981308157637893214090552534408589408121859898481114389650005964960521256960000000000000000000000000000)
   (printf "120! = ~a\n" final-result)))

