#lang s-exp "spl.rkt"

(let/spl (int x 10)
  (seq/spl
   (let/spl
    ((-> (int int) int)
     foo
     (lambda/spl ([int a] [int b]) int (add-int/spl a b)))
    (println/spl  (foo x 2)))
   (and/spl #f #t)))
