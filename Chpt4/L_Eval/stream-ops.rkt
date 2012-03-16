#lang racket

(provide
  display-stream
  interleave-delayed
  singleton-stream
  stream-append-delayed
  stream-flatmap
  the-empty-stream
  simple-stream-flatmap
  singleton-stream?
  stream-join)

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

(define (stream-join s1 s2)
  (if (or (stream-empty? s1)
          (stream-empty? s2))
    the-empty-stream
    (stream-flatmap (lambda (e1)
                      (stream-map (lambda (e2)
                                    (list e1 e2))
                                  s2))
                      s1)))

;; test function

(define (stream-print num-values s1)
  (if (and (> num-values 0) (not (stream-empty? s1)))
    (begin (printf "~a\n" (stream-first s1))
           (stream-print (- num-values 1)
                         (stream-rest s1)))
    (printf "End of stream\n")))

;(define s1 (stream 'a 'b 'c 'd 'e))
;(define s2 (stream 1 2 3 4 5))
;(define result (stream-join s1 s2))
;(stream-print 30 result)
