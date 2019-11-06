#lang racket

(provide from-k-ary
         to-k-ary)

(define (from-k-ary n k)
  (define (helper res n cnt)
    (if (= n 0)
        res
        (helper (+ (* (expt k cnt) (remainder n 10)) res) (quotient n 10) (+ cnt 1))))
  (helper 0 n 0))

(define (to-k-ary n k)
  (define (helper res n cnt)
    (if (< n k)
        (+ (* (expt 10 cnt) n) res)
        (helper (+  (* (expt 10 cnt) (remainder n k)) res) (quotient n k) (+ cnt 1))))
  (helper 0 n 0))