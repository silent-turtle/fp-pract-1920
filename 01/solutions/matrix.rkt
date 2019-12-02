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

; 00.
(define (all? p? xs)
  (foldl (lambda (x y) (and x y)) (and) (map p? xs)))

; 01.
(define (any? p? xs)
  (foldl (lambda (x y) (or x y)) (or) (map p? xs)))

; 02.
(define (concat xss)
  (foldr append '() xss))

; 03.
(define (rows xss) xss)

; 04.
(define (cols xss)
  (if (null? (cdar xss))
      (list (map car xss))
      (cons (map car xss) (cols (map cdr xss)))))

; 05.
(define (matrix-ref xss i j)
  (list-ref (list-ref xss i) j))

; 06.
(define (set xs i x)
  (define (map-index op nv a b f next l)
    (if (null? l)
        nv
        (op (f a (car l)) (map-index op nv (next a) b f next (cdr l)))))

  (map-index cons '() 0 (length xs)
             (lambda (n head) (if (= n i) x head)) (lambda (x) (+ x 1)) xs))

; 07.
(define (place xss i j x)
  (define (map-index op nv a b f next l)
    (if (null? l)
        nv
        (op (f a (car l)) (map-index op nv (next a) b f next (cdr l)))))

  (map-index cons '() 0 (length xss)
             (lambda (n head) (if (= n i) (set head j x) head)) (lambda (x) (+ x 1)) xss))

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
  (if (or (null? xs)
          (null? ys))
      '()
      (cons (f (car xs) (car ys)) (zip-with f (cdr xs) (cdr ys)))))

; 13.
(define (zip-matrix xss yss)
  (if (or (null? xss)
          (null? yss))
      '()
      (cons (zip-with cons (car xss) (car yss)) (zip-matrix (cdr xss) (cdr yss)))))