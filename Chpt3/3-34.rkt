#lang racket

; The problem is, the multiplier assumes all connectors correspond to different arguments.
; So if you give b, then it will not compute a for you, although mathematically there is
; enouch information to find a.

(define (squarer a b)
  (multiplier a a b))

(define x (make-connector))
(define y (make-connector))
(probe "squaree" x)
(probe "squared" y)
(squarer x y)
;(set-value! x 2 'user)
;(forget-value! x 'user)

(set-value! y 4 'user)
(set-value! x 3 'user)