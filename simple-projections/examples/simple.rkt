#lang s-exp "../simple-projections.rkt"

(simple-projections

 (define-projection A
   (define/proj x 1)
   (if/proj
    (eq? x 1)
    (begin/proj
      (select!/proj B 'OK)
      (send!/proj B 1))
    (begin/proj
      (select!/proj B 'KO)
      (send!/proj B 2))))

 (define-projection B
   (define/proj y 0)
   (merge/proj
    (begin/proj
      (branch?/proj A
       ['OK (begin/proj (recv?/proj A y))]))
    (begin/proj
      (branch?/proj A
       ['KO (begin/proj (recv?/proj A y))])))
   (expr-local/proj (println y))))
