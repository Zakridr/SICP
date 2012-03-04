#lang racket

(define (half-adder a b s c)
  (let ((d (make-wire)) 
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    ’ok))

(define (full-adder a b c-in sum c-out) 
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1) 
    (half-adder a s sum c2) 
    (or-gate c1 c2 c-out) 
    ’ok))

;As - list of Aks
;Bs - list of Bks
;C - one wire

(define (ripple-carry-adder As Bs Ss Cout)
  (let ((Cin (make-wire)))
    (if (null? (cdr As))
        (set-signal! Cin 0)
        (ripple-carry-adder (cdr As) (cdr Bs) (cdr Ss) Cin))
    (full-adder (car As) (car Bs) Cin (car Ss) Cout)))

; delay is: n(2*max(2and + inv, and + or) + or) = n(2*(max(and + inv, or) + and) + or)