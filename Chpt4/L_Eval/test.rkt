#lang racket

; takes any number of arguments (including 0!)
(define (foo . x)
  x)

(define g (lambda w
            (let ((arg-num (length w)))
              (cond ((= arg-num 0)
                     g)
                    ((= arg-num 1)
                     (lambda (x)
                       (+ (car w)
                          x)))
                    (else
                      (+ (car w)
                         (cadr w)))))))

((g 1) 2)
(g 1)
(g 1 2 3)
