#lang racket

(define (non-empty-els lst)
  (cond ((null? lst) 
         empty)
        ((null? (car lst))
         (non-empty-els (cdr lst)))
        (else
          (cons (car lst)
                (non-empty-els (cdr lst))))))

(printf "expecting empty:\n")
(non-empty-els (list '() '() '()))
(printf "expecting (1 2 3)")
(non-empty-els (list 1 2 3))

