#lang s-exp "../simple-choreographies.rkt"

(define-chor [Seller Buyer]
  (define-local Buyer title "How to Program")
  (define-local Buyer address "123 Street Street, City State, 12345")
  (define-local Buyer cost 0)
  (define-local Buyer recieve-date "")
  (define-local Seller catalog (make-hash (list '("How to Program" . 35))))
  (define-local Seller request "")
  (define-local Seller send-to "")
  (define-local Seller purchase
    (lambda (title address)
      (printf "Purchased '~a'. Sent to ~a.\n" title address)
      "July 10, 2024"))


  (com-> [Buyer title] [Seller request])
  (com-> [Seller (hash-ref catalog request)] [Buyer cost])
  (if/chor
   (Buyer (< cost 50))
   (begin/chor
     (sel-> [Buyer 'Buy] [Seller]
            (begin/chor
              (com-> [Buyer address] [Seller send-to])
              (com-> [Seller (purchase request send-to)]
                     [Buyer recieve-date]))))
   (begin/chor
     (sel-> [Buyer 'Do-Not-Buy] [Seller]
            (begin/chor
              (expr-local Buyer (println "Too expensive"))
              (expr-local Seller (println "Sorry to hear")))))))
