#lang racket

(provide from-numeral
         to-numeral
         plus
         mult
         pred)

(define zero (lambda (f v) v))
(define one (lambda (f v) (f v)))
(define two (lambda (f v) (f (f v))))

(define (succ n)
  (lambda (f v)
    (f (n f v))))

(define (from-numeral n)
  (define num 0)
  (define fun (lambda (x) (+ x 1)))
  (define (helper current cnt)
    (if (= (current fun num) (n fun num))
        cnt
        (helper (succ current) (+ cnt 1))))
  (helper zero 0))
    

(define (to-numeral n)
  (define (helper res current)
    (if (= current n)
        res
        (helper (succ res) (+ current 1))))
  (helper zero 0))
  

(define (plus n m)
  (lambda (f v) (m f (n f v))))


(define (mult n m)
  (lambda (f v) (m (lambda (v) (n f v)) v)))

(define (pred n)
  (lambda (f v)
    (if (= (from-numeral n) 0)
        (zero f v)
        ((to-numeral (- (from-numeral n) 1)) f v))))