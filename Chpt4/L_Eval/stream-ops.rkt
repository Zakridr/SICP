#lang racket

(provide
  display-stream
  interleave-delayed
  singleton-stream
  stream-append-delayed
  stream-flatmap
  the-empty-stream
  simple-stream-flatmap
  singleton-stream?)

(define the-empty-stream '())

(define (display-stream s)
  (stream-for-each (lambda (item)
                     (newline)
                     (display item))
                   s))

(define (stream-append-delayed s1 delayed-s2)
  (if (stream-empty? s1)
    (force delayed-s2)
    (stream-cons
      (stream-first s1)
      (stream-append-delayed (stream-rest s1) delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-empty? s1)
    (force delayed-s2)
    (stream-cons
      (stream-first s1)
      (interleave-delayed (force delayed-s2)
                          (delay (stream-rest s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-empty? stream)
    the-empty-stream
    (interleave-delayed
      (stream-first stream)
      (delay (flatten-stream (stream-rest stream))))))

(define (singleton-stream x)
  (stream-cons x the-empty-stream))

(define (simple-stream-flatmap proc s)
  (simple-flatten (stream-map proc s)))

(define (simple-flatten stream)
  (stream-map (lambda (s) (stream-first s))
              (stream-filter (lambda (s) (not (stream-empty? s))) 
                             stream)))

(define (singleton-stream? s)
  (and (not (stream-empty? s))
       (stream-empty? (stream-rest s))))
