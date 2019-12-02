#lang racket

(provide all?
         any?
         concat
         rows
         cols
         matrix-ref
         set
         place
         diag
         diags
         map-matrix
         filter-matrix
         zip-with
         zip-matrix)
; the provide "exports" these functions

(define even? (lambda (x) (= (remainder x 2) 0)))

(define matrix '((1 2 3) (4 5 6) (7 8 9)))

; 00.
(define (all? p? xs)
  (foldl (lambda (x y) (and x y)) (and) (map p? xs)))

; 01.
(define (any? p? xs)
  (foldl (lambda (x y) (or (p? x) y)) (or) xs))

; 02.
(define (concat xss)
  (foldr append '() xss))

; 03.
(define (rows xss) xss)

; 04.
(define (cols xss)
  (if (null? (cdar xss))
      (list (map car xss))
      (append (list (map car xss))
              (cols (map cdr xss)))))

; 05.
(define (matrix-ref xss i j)
  (list-ref (list-ref xss i) j))

; 06.
(define (set xs i x) 
  (cond ((null? xs) xs)
        ((= i 0) (cons x (cdr xs)))
        (else (cons (car xs) (set (cdr xs) (- i 1) x)))))
  
; 07.
(define (place xss i j x)
  (cond ((null? xss) xss)
        ((= i 0) (cons (set (car xss) j x) (cdr xss)))
        (else (cons (car xss) (place (cdr xss) (- i 1) j x)))))

; 08.
(define (diag xss)
  (if (null? (cdar xss))
      (cons (caar xss) '())
      (cons (caar xss) (diag (cdr (map cdr xss))))))

; 09.
(define (diags xss)
  (list (diag xss) (diag (map reverse xss))))

; 10.

(define (map-matrix f xss)
  (map (lambda (x) (map f x)) xss))

; 11.
(define (filter-matrix p? xss)
  (map (lambda (x) (filter p? x)) xss))

; 12.
(define (zip-with f xs ys)
  (apply map f (list xs ys)))

; 13.
(define (zip-matrix xss yss)
  (apply map (lambda (xs ys) (zip-with cons xs ys)) (list xss yss)))