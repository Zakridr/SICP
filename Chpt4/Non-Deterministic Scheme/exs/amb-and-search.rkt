(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-elemenet-of (cdr items))))

; 4.35
; an-integer-between int a * int b -> int c, where a<=c<=b

(define (an-integer-between a b)
  (require (<= a b))
  (amb a (an-integer-between (+ a 1) b)))

(define (an-integer-starting-from a)
  (amb a (an-integer-starting-from (+ a 1))))

; pyth proc:


; 4.36

(define (a-pythagorean-triple)
  (let* ((k (an-integer-starting-from 1))
         (j (an-integer-between 1 k))
         (i (an-integer-between 1 j)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))

; 4.37

(define (pyth-trp-btn low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

; the original algorithm explored O((h-l)^3) possibilities.
; This one explores O((h-l)^2) possibilities. -> more efficient

; non-deterministic program examples

(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper) 
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

; 4.38
; with the relaxtion of the not |s-f| = 1 condition, there are 5 solutions

; 4.39
; can analyze by determing how many times each constraint fires (assuming it were after
; the properly sorted constraints thus far) and order that way. Or maybe that's not the best
; way to order...

; new order:
;(require (distinct? (list baker cooper fletcher miller smith)))
;(require (> miller cooper))
;(require (not (= cooper 1)))
;(require (not (= fletcher 5)))
;(require (not (= fletcher 1)))
;(require (not (= (abs (- smith fletcher)) 1)))
;(require (not (= (abs (- fletcher cooper)) 1)))
;(require (not (= baker 5)))
; the b != 5 condition never fires. 

; 4.40

(define (multiple-dwelling)
  (let ((miller (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (require (not (= cooper 1)))
    (let ((fletcher (amb 1 2 3 4 5)))
      (require (not (= fletcher 5)))
      (require (not (= fletcher 1)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4 5)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (require (not (= baker 5)))
          (list (list 'baker baker)
                (list 'cooper cooper) 
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

; 4.41
; an ordinary scheme program to solve this one

(define (multiple-dwelling)
  (define guess-stream
    (stream-cons (list 1 1 1 1 1)
                 (stream-map inc-guess guess-stream)))

  (define (inc-guess guess)
    (cond ((empty? guess)
           (error "Overflow - INC-GUESS" guess))
          ((= 5 (car guess))
           (cons 1 (inc-guess (cdr guess))))
          (else
            (cons (+ 1 (car guess))
                  (cdr guess)))))

  (define (distinct? lst)
    (define (list-iter lst accum)
      (cond ((empty? lst) true)
            ((member (car lst) accum)
             false)
            (else
              (list-iter (cdr lst) (cons (car lst)
                                         accum)))))
    (list-iter lst '()))

  (define (conditions guess)
    (let ((b (list-ref guess 0))
          (c (list-ref guess 1))
          (f (list-ref guess 2))
          (m (list-ref guess 3))
          (s (list-ref guess 4)))
      (and (distinct? guess)
           (not (or (= b 5)
                    (= c 1)
                    (= f 1)
                    (= f 5)
                    (= 1 (abs (- f c)))
                    (= 1 (abs (- f s)))))
           (< c m))))

  (stream-first (stream-filter conditions guess-stream)))

; 4.42
; use amb to solve the liars guy

(define (liars)
  (let ((b (amb 1 2 3 4 5))
        (e (amb 1 2 3 4 5))
        (j (amb 1 2 3 4 5))
        (k (amb 1 2 3 4 5))
        (m (amb 1 2 3 4 5)))
    (define (xor p q)
      (if p
        (if (not q)
          true
          false)
        (if q
          true
          false)))
    (define (claim g1 v1 g2 v2)
      (xor (= g1 v1) (= g2 v2)))
    (require (distinct? (list b e j k m)))
    (require (claim k 2 b 3))
    (require (claim e 1 j 2))
    (require (claim j 3 e 5))
    (require (claim k 2 m 4))
    (require (claim m 4 b 1))
    (list (list 'b b)
          (list 'e e)
          (list 'j j) 
          (list 'k k)
          (list 'm m))))

; 4.43

(define (boats-and-daughters)
  (let ((g 'g)
        (l 'l)
        (r 'r)
        (m 'm)
        (ma 'ma)
        (cd 'cd)
        (mh 'mh)
        (bh 'bh)
        (dp 'dp)
        (mm 'mm)
        (daughters (list g
                         l
                         r
                         m
                         ma))
        (fathers (list cd
                       mh
                       bh
                       dp
                       mm)))
    (let ((b-o (list (list g bh)
                     (list l mm)
                     (list r mh)
                     (list m cd)
                     (list ma dp)))
          (d-f1 (list g (amb fathers))))
      (require (not (eq? (car (cdr d-f1)) bh)))
      (let ((d-f2 (list (amb daughters) dp)))
        (require (member? (list (car (cdr d-f1))
                                (car d-f2))
                          b-o))
        (require (not (eq? (car d-f2) ma)))
        (let ((d-f3 (list ma mm))
              (d-f4 (list (amb daughters) (amb fathers))))
          (require (not (member? d-f4 b-o)))
          (let ((d-f5 (list (amb daughers) (amb fathers))))
            (require (not (member? d-f5 b-o)))
            (require (distinct? (map (lambda (x) (car x)) (list d-f1 d-f2 d-f3 d-f4 d-f5))))
            (require (distinct? (map (lambda (x) (car (cdr x))) (list d-f1 d-f2 d-f3 d-f4 d-f5))))
            (list d-f1 d-f2 d-f3 d-f4 d-f5)))))))


; cleaner

(define (boats-and-daughters)
  (let ((g 'g)
        (l 'l)
        (r 'r)
        (m 'm)
        (ma 'ma)
        (cd 'cd)
        (mh 'mh)
        (bh 'bh)
        (dp 'dp)
        (mm 'mm))
    (let
      ((daughters (list g
                        l
                        r
                        m
                        ma))
       (fathers (list cd
                      mh
                      bh
                      dp
                      mm)))
      (let ((b-o (list (list g bh)
                       (list l mm)
                       (list r mh)
                       (list m cd)
                       (list ma dp)))
            (d-f1 (list g (amb fathers)))
            (d-f2 (list (amb daughters) dp))
            (d-f3 (list ma mm))
            (d-f4 (list m bh))
            (d-f5 (list (amb daughters) (amb fathers))))

        (require (not (eq? (cadr d-f1) bh)))
        (require (not (eq? (car d-f2) ma)))
        (require (distinct? (list (car d-f1)
                                  (car d-f2)
                                  (car d-f3)
                                  (car d-f4)
                                  (car d-f5))))
        (require (distinct? (list (cadr d-f1)
                                  (cadr d-f2)
                                  (cadr d-f3)
                                  (cadr d-f4)
                                  (cadr d-f5))))
        (list d-f1 d-f2 d-f3 d-f4 d-f5)))))

; damn 4.43 is soo painful.
; here is 4.44. tested with matt might's continuation stuff

(define (8-queens)
  (define (same-row? q1 q2)
    (= (car q1) (car q2)))
  (define (same-col? q1 q2)
    (= (cadr q1) (cadr q2)))
  (define (same-diag? q1 q2)
    (= (- (car q1) (cadr q1)) (- (car q2) (cadr q2))))
  (define 1-8 (list 1 2 3 4 5 6 7 8))
  (define (picker)
    (list (amb 1-8) (amb 1-8)))
  (define (clear? q1 q2)
    (not (or (same-row? q1 q2)
             (same-col? q1 q2)
             (same-diag? q1 q2))))
  (define (get-queens qlst length)
    (let ((q (picker)))
      (require (andmap (lambda (x) (clear? x q))
                       qlst))
      (if (= length 7)
        (cons q qlst)
        (get-queens (cons q qlst)
                    (+ length 1)))))
  (get-queens '() 0))

(define (print-queens lst)
  (define (pretty-printer coord)
    (if (= 8 (cadr coord))
      (if (member coord lst)
        (printf "x\n")
        (printf "~a\n" coord))
      (if (member coord lst)
        (printf "x     ")
        (printf "~a " coord))))
  (let ((board (build-list 64 (lambda (x) (list (+ 1 (quotient x 8))      
                                                (+ 1 (modulo x 8)))))))
    (map pretty-printer board)))

(define queens (8-queens))
(print-queens queens)
