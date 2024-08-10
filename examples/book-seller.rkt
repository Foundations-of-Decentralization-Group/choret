#lang racket

(require choret rackunit)

;; The book seller example from the Pirouette paper
(chor (Buyer Buyer-2 Seller)
      (define (at Buyer book-title) "Alice's Adventures in Wonderland")
      (define (at Buyer budget) 15)

      (define (at Seller prices)
        (make-hash (list (cons "Alice's Adventures in Wonderland" 20))))
      (define (at Seller availability)
        (make-hash
         (list (cons "Alice's Adventures in Wonderland" "January 1, 1970"))))
      (define (at Seller get-delivery-date)
        (at Seller (lambda (b) (hash-ref availability b))))

      (define Bookseller
        (lambda (F)
          (let ([(at Seller b) (~> (at Buyer book-title) Seller)])
            (let ([(at Buyer decision) (F (at Seller (hash-ref prices b)))])
              (if (at Buyer decision)
                  (sel~> Buyer
                         [Seller
                          'L
                          (~> (at Seller (get-delivery-date b)) Buyer)])
                  (sel~> Buyer
                         [Seller
                          'R
                          (at Buyer #f)]))))))

      (define Simple-Decision
        (lambda ((at Seller p))
          (let ([(at Buyer p) (~> (at Seller p) Buyer)])
            (at Buyer (< p budget)))))

      (define Contrib-Decision
        (lambda ((at Seller p))
          (let ([(at Buyer p) (~> (at Seller p) Buyer)])
            (let ([(at Buyer-2 p) (~> (at Seller p) Buyer-2)])
              (let ([(at Buyer contrib) (~> (at Buyer-2 (/ p 2)) Buyer)])
                (at Buyer (< (- p contrib) budget)))))))

      (let ([(at Buyer s) (Bookseller Simple-Decision)]
            [(at Buyer c) (Bookseller Contrib-Decision)])
        (at Buyer
            (begin
              (check-equal? s #f)
              (println s)))
        (at Buyer
            (begin
              (check-equal? c "January 1, 1970")
              (println c)))))
