(define (prime? n)
  (define (prime-iter curr)
    (cond ((< n (* curr curr))
           true)
          ((= (modulo n curr) 0)
           false)
          (else
            (prime-iter (+ curr 1)))))
  (prime-iter 2))

'start
(define (prime-sum-pair lst1 lst2)
  (let ((first (an-element-of lst1))
        (second (an-element-of lst2)))
    (assert (prime? (+ first second)))
    (list first second)))

'end
(let ((pairs '()))
  (if-fail
    (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
      (permanent-set! pairs (cons p pairs))
      (amb))
    pairs))

; returns the prime pairs, of course!
; consed into a nice list of pairs
