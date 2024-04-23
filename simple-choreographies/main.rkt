#lang s-exp "simple-choreographies.rkt"

(require racket/base)

(define-chor [Buyer Seller]
  (com-> [Buyer 12] [Seller x])
  (com-> [Seller 12] [Buyer x]))
