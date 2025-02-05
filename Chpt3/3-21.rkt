#lang racket

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue) (if (empty-queue? queue)
                                (error "FRONT called with an empty queue" queue)
                                (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    
    
    (cond ((empty-queue? queue) 
           (set-front-ptr! queue new-pair) 
           (set-rear-ptr! queue new-pair) queue)
          
          (else (set-mcdr! 
                 (rear-ptr queue) new-pair) 
                (set-rear-ptr! queue new-pair) queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

(define (print-queue q)
  (define (print-iter current)
    (if (null? current)
        '()
        (begin (print (mcar current))
               (print-iter (mcdr current)))))
  (print-iter (front-ptr q)))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(print-queue q)
(delete-queue! q)
(delete-queue! q)
