#lang racket

(require "3-3-5.rkt")

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))

(define (cv const)
  (let ((z (make-connector)))
    (constant const z)
    z))

(define (celsius->fahrenheit x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define a (make-connector))
;(define b (make-connector))
(define b (celsius->fahrenheit a))
(probe 'A a)
(probe 'B b)
(set-value! a 0 'user)
(forget-value! a 'user)
(set-value! b 50 'user)