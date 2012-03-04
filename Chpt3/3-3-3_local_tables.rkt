#lang racket

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (make-table)
  (let ((local-table (mcons '*table '())))
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
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define t (make-table))
((t 'insert-proc!) 1 2 'a)
((t 'insert-proc!) 1 1 'b)
((t 'lookup-proc) 1 1)
((t 'lookup-proc) 1 2)