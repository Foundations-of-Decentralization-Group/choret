#lang s-exp "spl.rkt"

(let/spl (int x 4)
  (let-syntax/spl
   (my-add
    (lambda (stx)
      (syntax-case stx (my-add)
        [(my-add args ...)
         (foldr
          (lambda (arg stx)
            #`(add-int/spl #,arg #,stx))
          #'0
          (syntax->list #'(args ...)))])))
    (let/spl (int y 5)
      (my-add 1 (my-add 1 1) 3 x y))))
