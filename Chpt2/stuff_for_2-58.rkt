
(length (list 1 2 3))

(define list1 (list 1 2))
(define list2 (list 3 4))
(define list3 (list 5 6))

(append list1 list2 list3)

(define sum '(+ 1 2 3 4 5))
sum
(append '(+) (cddr sum))

(define test (list 1 '+ 2))

(car test)
(cadr test)
(caddr test)

;(define (sum? x)
;  (define (find-plus x)
;    (if (pair? x)
;        (cond ((eq? (cadr x) '+) #t)
;              (else (find-plus (cddr x)))))
;    #f)
;  (find-plus x))

(define (sum? x)
  (if (< 2 (length x))
      (if (eq? (cadr x) '+) #t (sum? (cddr x)))
      #f) )

(define (splitter char ls)
  (define (find-char first-bit last-bit)
    (if (eq? char (car last-bit)) (list first-bit last-bit)
        (find-char (append first-bit (list (car last-bit) (cadr last-bit))) (cddr last-bit))))
  (find-char (list (car ls)) (cdr ls)))

(define (addend sum)
;  (define (find-plus first-bit last-bit)
;    (if (eq? '+ (car last-bit)) first-bit
;        (find-plus (append first-bit (list (car last-bit) (cadr last-bit))) (cddr last-bit))))
  (if (eq? (cadr sum) '+) (car sum)
      (car (splitter '+ sum))))

(define (augend sum)
  (define result (cadr (splitter '+ sum)))
  (if (= 2 (length result)) (cadr result) (cdr result)))
  

(define test-sum '(1 + 2 * 3 + 4))
(sum? test-sum)
(addend test-sum)
(augend test-sum)

