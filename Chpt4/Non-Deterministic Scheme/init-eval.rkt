#lang racket

(provide apply-primitive-procedure
         the-global-environment
         ambeval)
(require "evaluator.rkt"
         "eval-data-structs.rkt")

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

; additional primitive...
(define (distinct? lst)
  (define (list-iter lst accum)
    (cond ((empty? lst) true)
          ((member (car lst) accum)
           false)
          (else
           (list-iter (cdr lst) (cons (car lst)
                                      accum)))))
  (list-iter lst empty))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'list list)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'modulo modulo)
        (list 'sqrt sqrt)
        (list 'abs abs)
        (list '= =)
        (list 'not not)
        (list 'integer? integer?)
        (list '<= <=)
        (list '>= >=)
        (list '< <)
        (list '> >)
        (list 'eq? eq?)
        (list 'member? member)
        (list 'distinct? distinct?)
        (list 'cadr cadr)
        (list 'memq memq)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define input-prompt ";;; Amb-Eval input:")

(define output-prompt ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
          (try-again)
          (begin
            (newline)
            (display ";;; Starting a new problem ")
            (ambeval input
                     the-global-environment
                     ;; ambeval success
                     (lambda (val next-alternative)
                       (announce-output output-prompt)
                       (user-print val)
                       (internal-loop next-alternative))
                     ;; ambeval failure
                     (lambda ()
                       (announce-output
                        ";;; There are no more values of")
                       (user-print input)
                       (driver-loop)))))))
  (internal-loop
   (lambda ()
     (newline)
     (display ";;; There is no current problem")
     (driver-loop))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(define req-defn '(define (assert p)
                    (if (not p) (amb))))

(define map-defn '(define (map proc lst)
                    (if (null? lst)
                        '()
                        (cons (proc (car lst))
                              (map proc (cdr lst))))))
                    

(define int-btwn-defn '(define (an-integer-between a b)
                         (require (<= a b))
                         (amb a (an-integer-between (+ a 1) b))))

(define an-el-of-defn '(define (an-element-of items)
                         (assert (not (null? items)))
                         (amb (car items) (an-element-of (cdr items)))))

(define defns (list req-defn int-btwn-defn map-defn an-el-of-defn))

(map (lambda (defn)
       (ambeval defn
                the-global-environment
                ;; ambeval success
                (lambda (val next-alternative)
                  empty)
                ;; ambeval failure
                (lambda ()
                  empty)))
     defns)

(driver-loop)
