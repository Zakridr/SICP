#lang racket



(define (make-table same-key?)
  (let ((local-table (mcons '*table '())))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (mcar (mcar records))) (mcar records))
            (else (assoc key (mcdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                             (mcons (mcons key-2 value)
                                    (mcdr subtable)))))
            
            (set-mcdr! local-table
                       (mcons (mcons key-1 (mcons (mcons key-2 value) '() ))
                              (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup) lookup)
            ((eq? m 'insert!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define (compare num1 num2)
  (> 2 (abs (- num1 num2))))
(compare 1 2)
(compare 1 5)

(define t (make-table compare))

((t 'insert!) 1 1 'apple)
((t 'lookup) 2 1)