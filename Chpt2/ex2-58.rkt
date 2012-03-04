#lang racket

(define (variable? x) (symbol? x))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list  a1 '+ a2))))

(define (make-product a1 a2) 
  (cond ((or (=number? a1 0) (=number? a2 0)) 0) 
        ((=number? a1 1) a2)
        ((=number? a2 1) a1)
        ((and (number? a1) (number? a2)) (* a1 a2))
        (else (list a1 '* a2))))

(define (splitter char ls)
  (define (find-char first-bit last-bit)
    (if (eq? char (car last-bit)) (list first-bit last-bit)
        (find-char (append first-bit (list (car last-bit) (cadr last-bit))) (cddr last-bit))))
  (find-char (list (car ls)) (cdr ls)))

(define (sum? x)
  (if (< 2 (length x))
      (if (eq? (cadr x) '+) #t (sum? (cddr x)))
      #f) )

(define (addend sum)
  (if (eq? (cadr sum) '+) (car sum)
      (car (splitter '+ sum))))

(define (augend sum)
  (define result (cadr (splitter '+ sum)))
  (if (= 2 (length result)) (cadr result) (cdr result)))

(define (product? x)
  (if (< 2 (length x))
      (if (eq? (cadr x) '*) #t (product? (cddr x)))
      #f) )

(define (multiplier sum)
  (if (eq? (cadr sum) '*) (car sum)
      (car (splitter '* sum))))

(define (multiplicand sum)
  (define result (cadr (splitter '* sum)))
  (if (= 2 (length result)) (cadr result) (cdr result)))

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
        (else
         (error "unknown expression type - DERIV" exp))))

(deriv '(x + x * x * (x + y + z)) 'x)
(deriv '(1 * (x + 3) * x) 'x)