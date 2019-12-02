#lang racket

(provide winner
         play)

(require "matrix.rkt")
; You can use your matrix functions below, thanks to the "require" invocation above.

(define (id x) x)

; winner implementation that only detects draws right now.
; Put your own implementation here!
(define (check-all-if-all sym b)
  (any? (lambda (x) (eqv? x #t)) (map (lambda (xs) (all? (lambda (x) (eqv? x sym)) xs)) b)))

(define (check-all-if-any sym b)
  (any? (lambda (x) (eqv? x #t)) (map (lambda (xs) (any? (lambda (x) (eqv? x sym)) xs)) b)))

(define (winner b)
  (cond ((or (check-all-if-all "X" (rows b))
             (check-all-if-all "X" (cols b))
             (check-all-if-all "X" (diags b))) "X")
        ((or (check-all-if-all "O" (rows b))
             (check-all-if-all "O" (cols b))
             (check-all-if-all "O" (diags b))) "O")
        ((or (check-all-if-any #f (rows b))
             (check-all-if-any #f (cols b))
             (check-all-if-any #f (diags b))) #f)
        (else "D")))
        

  
; "Dumb" "AI", plays the "next" free spot, going left-to-right, top-to-bottom.
; Put your own implementation here!
(define (play curr-board curr-sign)
  (define (helper i j)
    (cond ((> i 2) #f)
          ((> j 2) (helper (+ i 1) 0))
          ((not (list-ref (list-ref curr-board i) j)) (cons i j))
          (else (helper i (+ j 1)))))
  (helper 0 0))
