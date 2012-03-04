#lang racket

; 2-d tables

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcar (mcar records))) (mcar records))
        (else (assoc key (mcdr records)))))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (assoc key-2 (mcdr subtable))))
          (if record
              (mcdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (assoc key-2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! subtable
                         (mcons (mcons key-2 value)
                                (mcdr subtable)))))
        (set-mcdr! table
                   (mcons (mcons key-1 (mcons (mcons key-2 value) '()))
                          (mcdr table)))))
  'ok)

; that's wrong
; list 1 2 3 = (cons 1 (cons 2 (cons 3 '())))