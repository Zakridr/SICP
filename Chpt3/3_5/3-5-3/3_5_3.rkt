#lang racket

(require "../3-5-2/3_5_2.rkt")
(provide prime?
         square
         print
         average
         sqrt-stream
         pi-stream
         euler-transform
         make-tableau
         accelerated-sequence
         int-pairs
         pairs
         interleave
         integers
         stream-map
         stream-print-n
         integral
         scale-stream
         add-streams)


(define (prime? p)
  (define (prime-iter p n)
    (cond ((> (square n) p) true)
          ((= (modulo p n) 0) false)
          ((prime-iter p (+ n 1)))))
  (prime-iter p 2))

(define (square x)
  (* x x))

(define (print x)
  (display x)
  (newline))

(define (average . nums)
  (/ (foldr + 0 nums) (length nums)))


(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-rest s)))))

(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-first
              (make-tableau transform s)))

(define hyper-pi-stream
  (accelerated-sequence euler-transform pi-stream))

; 3.64

(define (stream-limit s epsilon)
  (let ((first (stream-first s))
        (second (stream-first (stream-rest s))))
    (if (> epsilon (abs (- first second)))
        second
        (stream-limit (stream-rest s) epsilon))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

; 3.65

(define (ln2-summands n)
  (stream-cons (/ 1 n)
               (stream-map - (ln2-summands (+ n 1)))))
(define ln2-stream
  (partial-sums (ln2-summands 1)))

(define transform-ln2-stream
  (euler-transform ln2-stream))

(define hyper-ln2-stream
  (accelerated-sequence euler-transform ln2-stream))

; converge quite slowly..

(define (pairs s t)
  (stream-cons
   (list (stream-first s) (stream-first t))
   (interleave
    (stream-map (lambda (x) (list (stream-first s) x))
                (stream-rest t))
    (pairs (stream-rest s) (stream-rest t)))))

(define (interleave s1 s2)
  (if (stream-empty? s1)
      s2
      (stream-cons (stream-first s1)
                   (interleave s2 (stream-rest s1)))))

(define int-pairs
  (pairs integers integers))

(define prime-sum-pairs (stream-filter (lambda (pair)
                                    (prime? (+ (car pair) (cadr pair))))
                                  int-pairs))

(define (integral integrand initial-value dt)
  (define int
    (stream-cons initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)