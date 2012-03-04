#lang racket

; 3-47

(define (make-mutex)
  (let ((cell (mcons false '())))
    (define (the-mutex m)
      (cond ((eq? 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)
                 '()));retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-mcar! cell false))

(define (test-and-set! cell)
  (if (mcar cell)
      true
      (begin (set-mcar! cell true)
             false)))

(define (make-list-obj constructor n)
  (if (= n 0)
      '()
      (cons (constructor) (make-list-obj constructor (- n 1)))))

(define (massoc v lst)
  (cond ((null? lst)
         false)
        ((eq? v (mcar (mcar lst)))
         (mcar lst))
        (else (massoc v mcdr lst))))

(define (list->mlist lst)
  (if (null? lst)
      '()
      (mcons (car lst) (list->mlist (cdr lst)))))

;a.
(define (make-semaphore size)
  (let ((mutex-list (list->mlist (build-list size (lambda (i) (mcons 'free (make-mutex)))))))
    (define (get-?-mutex status)
      (massoc status mutex-list))
    (define (the-semaphore m)
      (cond ((eq? 'acquire)
             (let ((mutex-pair (get-?-mutex 'free)))
               (if mutex-pair
                   (begin
                     ((mcdr mutex-pair) 'acquire)
                     (set-mcar! mutex-pair 'busy))
                   (the-semaphore 'acquire))))
            ((eq? 'release)
             (let ((mutex-pair (get-?-mutex 'busy)))
               (if mutex-pair
                   (begin 
                     ((mcdr mutex-pair) 'release)
                     (set-mcar! mutex-pair 'free))
                   '())))))
    the-semaphore))

;b.
; false - means we're free
; true - means busy
; truth value - is-busy?
(define (make-at-semaphore size)
  (let ((cells (list->mlist (build-list size (lambda (i) (mcons false '()))))))
    (define (get-?-cell is-busy)
      (massoc is-busy cells))
    (define (the-semaphore m)
      (cond ((eq? 'acquire)
             (let ((free-cell (get-?-cell false)))
               (if free-cell
                   (test-and-set! free-cell)
                   (the-semaphore 'acquire))))
            ((eq? 'release)
             (let ((busy-cell (get-?-cell true)))
               (if busy-cell
                   (clear! busy-cell)
                   '())))))
    the-semaphore))
