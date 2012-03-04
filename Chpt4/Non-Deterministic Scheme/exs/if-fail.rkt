
(define (even? x)
  (cond ((= x 1)
         false)
        ((= x 0)
         true)
        (else
          (even? (- x 2)))))

(if-fail (let ((x (an-element-of '(1 3 5))))
           (require (even? x))
           x)
         'all-odd)

(if-fail (let ((x (an-element-of '(1 3 5 8))))
           (require (even? x))
           x)
         'all-odd)
