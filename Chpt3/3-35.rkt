#lang racket

(require "3-3-5.rkt")

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 - SQUARER"
                   (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (cond ((has-value? a)
               (let ((a-value (get-value a)))
            (set-value! b (* a-value a-value) me))))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else
           (error "Unknown request - SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define x (make-connector))
(define y (make-connector))
(probe "squaree" x)
(probe "squared" y)
(squarer x y)

(set-value! y 9 'user)
(forget-value! y 'user)

(set-value! x 3 'user)
(forget-value! x 'user)
(set-value! x 5 'user)
(forget-value! x 'user)

(set-value! y 25 'user)