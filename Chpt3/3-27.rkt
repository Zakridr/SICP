#lang racket
(define (make-table)
  (mcons '*table* '()))

(define (lookup key table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                   (mcons (mcons key value) (mcdr table)))))
  'ok)

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memoize (lambda (n)
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   (else (+ (memo-fib (- n 1))
                            (memo-fib (- n 2))))))))

(memo-fib 3)

(or #t (display "argh"))

; so or is a little funky.