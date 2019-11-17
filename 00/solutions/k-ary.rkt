#lang racket

(provide from-k-ary
         to-k-ary)

(define (transform-to-k-ary n k m)
  (define (helper res n cnt)
    (if (= n 0)
        res
        (helper (+  (* (expt m cnt)
                       (remainder n k))
                    res)
                (quotient n k)
                (+ cnt 1))))

  (helper 0 n 0))

(define (from-k-ary n k)
  (transform-to-k-ary n 10 k))

(define (to-k-ary n k)
  (transform-to-k-ary n k 10))

(to-k-ary 69217 7)
