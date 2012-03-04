#lang racket

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-exponentiation base exponent)
  (cond ((and (=number? base 0) (not (=number? exponent 0))) 0)
        ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        ((and (number? base) (number? exponent)) (expt base exponent))
        (else (list '** base exponent))))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

(define (make-product a1 a2) 
  (cond ((or (=number? a1 0) (=number? a2 0)) 0) 
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list '* a1 a2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))

(define (base expr) (cadr expr))
(define (exponent expr) (caddr expr))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product (exponent exp)
                                     (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type - DERIV" exp))))

(deriv '(+ x 3) 'x)
(define test '(* (+ x 3) 2))
(deriv test 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define test1 (make-exponentiation 3 2))
test
(deriv test1 'x)
(define test2 (make-exponentiation 'x 10))
(deriv test2 'x)
(define test3 (make-exponentiation 'y 5))
(deriv test3 'y)