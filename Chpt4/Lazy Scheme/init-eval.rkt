#lang racket

(provide apply-primitive-procedure)
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

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '= =)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list 'display display)
        (list 'newline newline)
        ;        (list 'foo (lambda (x) (+ x 1)))
        ;        (list 'map map)
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define input-prompt ";;; L-Eval input:")

(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

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


; testing adding some definitions to meta-circular evaluator
(define cons-def '(define (cons x y) (lambda (m) (m x y))))
(define car-def '(define (car l) (l (lambda (p q) p))))
(define cdr-def '(define (cdr l) (l (lambda (p q) q))))
(define list-ref-def '(define 
                        (list-ref items n) 
                        (if (= n 0) 
                            (car items) 
                            (list-ref (cdr items) (- n 1)))))
(define map-def '(define (map proc items)
                   (if (null? items)
                       '()
                       (cons (proc (car items))
                             (map proc (cdr items))))))
(define scale-list-def '(define (scale-list items factor)
                          (map (lambda (x) (* x factor))
                               items)))
(define add-lists-def '(define (add-lists l1 l2)
                         (cond ((null? l1) l2)
                               ((null? l2) l1)
                               (else (cons (+ (car l1)
                                              (car l2))
                                           (add-lists (cdr l1)
                                                      (cdr l2)))))))


(define defs (list cons-def car-def cdr-def list-ref-def map-def
                   scale-list-def add-lists-def))

(map (lambda (defn)
       (eval defn the-global-environment))
     defs)


(eval cons-def  the-global-environment)
(driver-loop)