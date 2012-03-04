#lang racket

(define a
  (let ((x 5))
    (+ x 1)))

(define (multer x)
  (lambda (y)
    (* x y)))

(define scaler-5 (multer 5))

(define lst1 (list 1 2 3 4 5 6))

(define square
  (λ (x) (* x x)))

(square 5)
(+ (square 5) 5)
(+ 1 1)
; I wonder what will happen now?
