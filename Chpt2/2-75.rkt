#lang scheme

; quite mysterious behaviour...

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op imag-part) y)
          ((eq? op magnitude) (sqrt (+ (square x) (square y))))
          ((eq? op angle) (atan y x))
          (else
           (error "Unknown op - MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unkown op - MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (square x) (* x x))

(define (apply-generic op arg) (arg op))

(define z (make-from-real-imag 1 1))
(apply-generic 'real-part z)

; we need to quote the function in this case... lame







