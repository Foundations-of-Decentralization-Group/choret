#lang s-exp "spl.rkt"

(let/spl (int x 5)
  (let/spl ((-> ((-> (int) int) int) int) apply
            (lambda/spl ([(-> (int) int) f] [int x]) int
                        (f x)))
    (apply
     (lambda/spl ([int x]) int (add-int/spl x 1))
     x)))
