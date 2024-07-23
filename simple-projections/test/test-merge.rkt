#lang racket/base

(require
 rackunit
 "../../util/check-syntax.rkt")

;;; Test cases where the code should be syntactically correct. The semantics of
;;; these programs are also tested.

(test-case
 "No syntax errors when merging if/proj"
 (check-not-syntax-error
  (require "../simple-projections.rkt" rackunit)
  (simple-projections

   (define-projection A
     (define/proj x 0)
     (merge/proj
      (if/proj
       (eq? x 0)
       (send!/proj B (+ x 1))
       (send!/proj B (+ x 2)))
      (if/proj
       (eq? x 0)
       (send!/proj B (+ x 1))
       (send!/proj B (+ x 2)))))

   (define-projection B
     (define/proj y 0)
     (recv?/proj A y)
     (expr-local/proj (check-equal? y 1))))))

(test-case
 (string-append
  "No syntax errors when merging branch?/proj")
 (check-not-syntax-error
  (require "../simple-projections.rkt" rackunit)
  (simple-projections

   (define-projection A
     (select!/proj B 'X)
     (send!/proj B 10))

   (define-projection B
     (define/proj x 0)
     (merge/proj
      (branch?/proj A
                    ['X (recv?/proj A x)]
                    ['Y (recv?/proj A x)])
      (branch?/proj A
                    ['X (recv?/proj A x)]
                    ['Y (recv?/proj A x)]))
     (expr-local/proj (check-equal? x 10))))))

;;; Test cases where the code should not be syntactically correct. This is to
;;; test that certain errors are properly caught at compile time.

(test-case
 "Syntax error due to mismatched send!/proj local expression forms"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections
   (define-projection A
     (define/proj x 0)
     (recv?/proj B x))

   (define-projection B
     (merge/proj
      (send!/proj A 1)
      (send!/proj A 2))))))

(test-case
 "Syntax error due to mismatched send!/proj process names"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections
   (define-projection A
     (define/proj x 0)
     (recv?/proj C x))

   (define-projection B
     (define/proj x 0)
     (recv?/proj C x))

   (define-projection C
     (merge/proj
      (send!/proj A 1)
      (send!/proj B 1))))))

(test-case
 "Syntax error due to mismatched recv?/proj identifiers"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections
   (define-projection A
     (send!/proj B 5))

   (define-projection B
     (define/proj x 0)
     (define/proj y 0)
     (merge/proj
      (recv?/proj A x)
      (recv?/proj A y))))))

(test-case
 "Syntax error due to mismatched recv?/proj process names"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections
   (define-projection A
     (send!/proj C 1))

   (define-projection B
     (send!/proj C 2))

   (define-projection C
     (define/proj x 0)
     (merge/proj
      (recv?/proj A x)
      (recv?/proj B x))))))

(test-case
 "Mismatched number of forms when merging two begin/chor forms"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections
   (define-projection A
     (define/proj x 0)

     (merge/proj
      (begin/proj
        (expr-local/proj (set! x (+ x 1))))
      (begin/proj
        (expr-local/proj (set! x (+ x 1)))
        (expr-local/proj (set! x (+ x 1)))))))))

(test-case
 "Syntax error due to mismatched names in nested merge"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections

   (define-projection A
     (define/proj x 0)
     (merge/proj
      (if/proj
       (eq? x 0)
       (send!/proj B (+ x 1))
       (send!/proj B (+ x 2)))
      (if/proj
       (eq? x 0)
       (send!/proj C (+ x 1))
       (send!/proj C (+ x 2)))))

   (define-projection B
     (define/proj y 0)
     (recv?/proj A y)
     (expr-local/proj (println y)))

   (define-projection C
     (define/proj y 0)
     (recv?/proj A y)
     (expr-local/proj (println y))))))

(test-case
 "Syntax error due to mismatched local expression in if/proj"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections

   (define-projection A
     (define/proj x 0)
     (merge/proj
      (if/proj
       (eq? x 0)
       (send!/proj B (+ x 1))
       (send!/proj B (+ x 2)))
      (if/proj
       (eq? x 1)
       (send!/proj B (+ x 1))
       (send!/proj B (+ x 2)))))

   (define-projection B
     (define/proj y 0)
     (recv?/proj A y)
     (expr-local/proj (println y))))))

(test-case
 "Synax error due to mismatched expression in if/proj"
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections

   (define-projection A
     (define/proj x 0)
     (merge/proj
      (if/proj
       (eq? x 0)
       (send!/proj B x)
       (send!/proj B x))
      (if/proj
       (eq? x 0)
       (recv?/proj B x)
       (recv?/proj B x))))

   (define-projection B
     (define/proj y 0)
     (recv?/proj A y)
     (expr-local/proj (println y))))))

(test-case
 (string-append
  "Syntax error due to merging branch/proj expressions with conflicting "
  "expressions for the same label")
 (check-has-syntax-error
  (require "../simple-projections.rkt")
  (simple-projections

   (define-projection A
     (select!/proj B 'X)
     (send!/proj B 1))

   (define-projection B
     (define/proj x 0)
     (define/proj y 0)
     (merge/proj
      (branch?/proj A
                    ['X (recv?/proj A x)]
                    ['Y (recv?/proj A x)])
      (branch?/proj A
                    ['X (recv?/proj A y)]
                    ['Y (recv?/proj A x)]))))))