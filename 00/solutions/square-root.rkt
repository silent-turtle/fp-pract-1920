#lang racket

(provide my-sqrt)
  
(define (my-sqrt x)
  (define (func y)
    (- y (/ (- (expt y 2) x) (* 2 y))))
  (define (helper res)
    (if (< (abs (- res (sqrt x))) 0.000001)
        (* res 1.0)
        (helper (func res))))
 (helper 10))