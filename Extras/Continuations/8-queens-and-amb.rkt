#lang racket

(define (current-continuation) 
  (call-with-current-continuation 
   (lambda (cc)
     (cc cc))))

; fail-stack : list[continuation]
(define fail-stack '())

; fail : -> ...
(define (fail)
  (if (not (pair? fail-stack))
      (error "back-tracking stack exhausted!")
      (begin
        (let ((back-track-point (car fail-stack)))
          (set! fail-stack (cdr fail-stack))
          (back-track-point back-track-point)))))

; amb : list[a] -> a
(define (amb choices)
  (let ((cc (current-continuation)))
    (cond
      ((null? choices)      (fail))
      ((pair? choices)      (let ((choice (car choices)))
                              (set! choices (cdr choices))
                              (set! fail-stack (cons cc fail-stack))
                              choice)))))

; (assert condition) will cause
; condition to be true, and if there
; is no way to make it true, then
; it signals and error in the program.
(define (assert condition)
  (if (not condition)
      (fail)
      #t))
(define (distinct? lst)
  ; check to ensure all elements are distinct
  (let ((so-far empty))
    (andmap (lambda (x)
              (begin
                (set! so-far (cons x so-far))
                (not (member x (cdr so-far)))))
            lst)))

; 4.43
; first pass
; separate the 'is a daughter' from the 'is a father' relationship
(define (daughters)
  (let ((m 'm)
        (g 'g)
        (l 'l)
        (mam 'ma)
        (r 'r)
        (cd 'cd)
        (mh 'mh)
        (bh 'bh)
        (dp 'dp)
        (mm 'mm))
    (let* ((fathers (list cd
                         mh
                         bh
                         dp
                         mm))
          (daughters (list m
                           g
                           l
                           mam
                           r))
          (ownrs-bs (list (list bh g)
                          (list mm l)
                          (list mh r)
                          (list cd m)
                          (list dp mam)))
          (mm-mam (list mm mam))
          (bh-m (list bh m))
          (?-g (list (amb fathers) g))
          (dp-? (list dp (amb daughters))))
      (assert (member (list (car ?-g) 
                            (cadr dp-?))
                      ownrs-bs))
      (assert (not (eq? (car ?-g)
                        bh)))
      (assert (not (eq? (cadr dp-?)
                        mam)))
      (let* ((?-? (list (amb fathers)
                       (amb daughters)))
             (fs-ds (list mm-mam
                          bh-m
                          ?-g
                          dp-?
                          ?-?)))
        (assert (distinct?
                  (map (lambda (x) (car x))
                       fs-ds)))
        (assert (distinct?
                  (map (lambda (x) (cadr x))
                       fs-ds)))
        fs-ds))))

; l's father is cd!
; this is 4.44

;(define (8-queens)

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
      (assert (andmap (lambda (x) (clear? x q))
                      qlst))
      (if (= length 7)
        (cons q qlst)
        (get-queens (cons q qlst)
                    (+ length 1)))))
  (get-queens empty 0))

(define (print-queens lst)
  (define (pretty-printer coord)
    (if (= 8 (cadr coord))
      (if (member coord lst)
        (printf "x\n")
        (printf "~a\n" coord))
      (if (member coord lst)
        (printf "   x  ")
        (printf "~a " coord))))
  (let ((board (build-list 64 (lambda (x) (list (+ 1 (quotient x 8))      
                                                (+ 1 (modulo x 8)))))))
    (map pretty-printer board)))
