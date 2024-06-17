#lang s-exp "spl.rkt"

(let/spl
 (int x 5)
 (let-syntax/spl
  (my-add
   (lambda (stx)
     (syntax-case stx (my-add)
       [(my-add arg1 arg2)
        #'(let/spl (int x 50) (add-int/spl arg1 arg2))])))
  (let/spl
   (int y 10)
   (my-add x y))))
