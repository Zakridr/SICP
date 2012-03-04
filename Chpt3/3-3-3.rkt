#lang racket

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

(define (print thingy)
  (display thingy)
  (display "\n"))

(define (print-table t)
  (define (table-iter current)
    (if (null? current)
        'done
        (let ((key-value (mcar current)))
          (display "Key: ")
          (print (mcar key-value))
          (display "Value: ")
          (print (mcdr key-value))
          (table-iter (mcdr current)))))
  (table-iter (mcdr t)))

(define (make-table)
  (mcons '*table* '()))


