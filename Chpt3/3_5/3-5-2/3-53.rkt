#lang racket

; 3.53

(define (stream-map proc . argstreams)
  (if (stream-empty? (car argstreams))
      empty-stream
      (stream-cons (apply proc (map stream-first argstreams))
                   (apply stream-map (cons proc (map stream-rest argstreams))))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define s (stream-cons 1 (add-streams s s)))

; 1, 2, 4,.... powers of 2

