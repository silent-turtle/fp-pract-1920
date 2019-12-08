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
  (define (get-next-unused-slot board i j)
      (cond ((> j 2) (get-next-unused-slot board (+ i 1) 0))
            ((> i 2) #f)
            ((not (matrix-ref board i j)) (cons i j))
            (else (get-next-unused-slot board i (+ j 1)))))
  
  (define (max-board board-a board-b)
    (if (>= (car board-a) (car board-b))
        board-a
        board-b))
  
  (define (min-board board-a board-b)
    (if (<= (car board-a) (car board-b))
        board-a
        board-b))

  (define (get-best-curr-board board from-i from-j curr-i curr-j sign)
    (define result (winner board))
    (define symbol (if sign "X" "O"))
    (define next-unused-slot (get-next-unused-slot board from-i from-j))
    (define next-next-unused-slot (if (not (eq? next-unused-slot #f))
                                      (get-next-unused-slot board (+ (car next-unused-slot) 1) (+ (cdr next-unused-slot) 1))
                                      #f))
    (define new-board (if (not (eq? next-unused-slot #f))
                          (place board (car next-unused-slot) (cdr next-unused-slot) symbol)
                          board))

    ; checking if the current board can be evaluated
    (cond ((and curr-sign
                (eqv? result "X")) (cons 1
                                         (cons curr-i curr-j)))
          ((eqv? result "D") (cons 0
                                   (cons curr-i curr-j)))
          ((and (not curr-sign)
                (eqv? result "O")) (cons -1
                                         (const curr-i curr-j)))
          (else (cond ((eq? next-next-unused-slot #f)
                       (get-best-curr-board new-board 0 0 (car next-unused-slot) (cdr next-unused-slot) (not sign)))
                      ; if it's our turn we calculate what the result would be if we place our symbol on the current position
                      ; then check what the result from placing the symbol on the other free position would be
                      ; then getting the maximum value
                      ((eq? sign curr-sign)
                       (max (get-best-curr-board new-board 0 0 (car next-unused-slot) (cdr next-unused-slot) (not sign))
                            (get-best-curr-board board (car next-unused-slot) (cdr next-unused-slot) (car next-next-unused-slot) (cdr next-next-unused-slot) sign)))
                      ; it's the same for the oponent's turn only we get the minimum value
                      (else
                       (min (get-best-curr-board new-board 0 0 (car next-unused-slot) (cdr next-unused-slot) (not sign))
                            (get-best-curr-board board (car next-unused-slot) (cdr next-unused-slot) (car next-next-unused-slot) (cdr next-next-unused-slot) sign)))))))


  (define first-unused (get-next-unused-slot curr-board 0 0))

  (cdr (get-best-curr-board curr-board 0 0 (car first-unused) (cdr first-unused) curr-sign)))