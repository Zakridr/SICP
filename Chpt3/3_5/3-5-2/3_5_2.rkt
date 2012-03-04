#lang racket

(provide stream-print-n
         scale-stream
         stream-map
         mul-streams
         add-streams
         partial-sums
         merge
         mul-series
         integers)

(define (print x)
  (display x)
  (newline))

(define (stream-print-n s n)
  (if (= n 0)
      'done
      (begin
        (print (stream-first s))
        (stream-print-n (stream-rest s)
                        (- n 1)))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

; 3.54

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons (apply proc (map stream-first argstreams))
                   (apply stream-map (cons proc (map stream-rest argstreams))))))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (stream-cons 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers (stream-cons 1 (add-streams ones integers)))

(define factorials (stream-cons 1 (mul-streams integers factorials)))

; 3.55

(define (partial-sums s)
  (stream-cons (stream-first s) (add-streams (stream-rest s) (partial-sums s))))

(define triangles (partial-sums integers))

; 3.56

(define (merge s1 s2)
  (cond ((stream-empty? s1) s2)
        ((stream-empty? s2) s1)
        (else
         (let ((s1car (stream-first s1))
               (s2car (stream-first s2)))
           (cond ((< s1car s2car)
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) s2)))
                 ((> s1car s2car)
                  (stream-cons
                   s2car
                   (merge s1 (stream-rest s2))))
                 (else
                  (stream-cons
                   s1car
                   (merge (stream-rest s1) (stream-rest s2)))))))))

(define ham-seq (stream-cons 1 (merge (scale-stream ham-seq 2)
                                      (merge (scale-stream ham-seq 3)
                                             (scale-stream ham-seq 5)))))

(define twos-threes (stream-cons 1 (merge (scale-stream twos-threes 2)
                                          (scale-stream twos-threes 3))))

; 3.58

(define (expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))


; returns the decimal expansion of the number in the given base

; 3.59

(define (integrate-series s)
  (stream-cons (stream-first s)
               (mul-streams (stream-rest s) (stream-map (lambda (x) (expt x -1)) (stream-rest integers)))))

(define exp-series (stream-cons 1 (integrate-series exp-series)))
(define cosine-series
  (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))
(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))

(define (mul-series s1 s2)
  (let ((a0 (stream-first s1))
        (b0 (stream-first s2)))
    (stream-cons (* a0 b0)
                 (add-streams (add-streams (scale-stream (stream-rest s1) b0) 
                                           (scale-stream (stream-rest s2) a0))
                              (stream-cons 0 
                                           (mul-series (stream-rest s1)
                                                       (stream-rest s2)))))))

(define (invert-unit-series s)
  (stream-cons 1 (scale-stream (mul-series (stream-rest s)
                                           (invert-unit-series s))
                               -1)))

(define (invert-series s)
  (let ((const-term (stream-first s)))
    (if (= 0 const-term)
        (error "zero constant term - INVERT-SERIES" s)
        (let ((one-over-c (/ 1 const-term)))
          (scale-stream (invert-unit-series (scale-stream s one-over-c)) one-over-c)))))

(define (div-series s1 s2)
  (mul-series s1 (invert-series s2)))



; defns for testing

(define ints? (mul-series ones ones))
(define one? (add-streams (mul-series cosine-series cosine-series) 
                          (mul-series sine-series sine-series)))
(define cos-sq (mul-series cosine-series cosine-series))
(define sin-sq (mul-series sine-series sine-series))
(define tan-sq (div-series sine-series cosine-series))

(define zeroes (stream-cons 0 zeroes))

(define base (stream-cons 1 (stream-cons -1 zeroes)))
(define ones? (invert-unit-series base))

(define twos (stream-cons 2 twos))
(define twos-inv (invert-series twos))
