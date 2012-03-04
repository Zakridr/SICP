#lang racket

(provide self-evaluating?
         variable?
         quoted?
         text-of-quotation
         assignment?
         definition?
         if?
         lambda?
         begin?
         cond?
         cond->if
         application?
         no-operands?
         if-predicate
         if-consequent
         if-alternative
         last-exp?
         first-exp
         rest-exps
         assignment-variable
         assignment-value
         definition-variable
         definition-value
         tagged-list?
         lambda-parameters
         lambda-body
         begin-actions
         operator
         first-operand
         rest-operands
         operands
         let?
         let->combination)
;(require "eval-data-structs.rkt")
;variables, self-evaluators (numbers & strings)
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

;quotations
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;assignemnt
(define (assignment? exp)
  (tagged-list? exp 'set))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; definitions
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; lambdas
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))

(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; ifs

(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin

(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))

(define (first-exp seq) (car seq))

(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; proc application

(define (application? exp) (pair? exp))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))

(define (first-operand ops) (car ops))

(define (rest-operands ops) (cdr ops))

; cond

(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last - COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

; let

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-binds-body exp)
  (cdr exp))

(define (let-binds let-stuff)
  (car let-stuff))

(define (let-body let-stuff)
  (cadr let-stuff))

(define (let-vars let-binds)
  (if (null? let-binds)
      empty
      (cons (caar let-binds)
            (let-vars (cdr let-binds)))))

(define (let-vals let-binds)
  (if (null? let-binds)
      empty
      (cons (cadar let-binds)
            (let-vals (cdr let-binds)))))

(define (let->combination exp)
  (if (named-let? exp)
      (nlet->let (cadr exp) (caddr exp) (cadddr exp))
      (expand-let (let-binds (let-binds-body exp))
                  (let-body (let-binds-body)))))

(define (expand-let binds body)
  (let ((vars (let-vars binds))
        (vals (let-vals binds)))
    (cons (make-lambda vars body)
          vals)))

; named let

(define (make-let bindings body)
  (cons 'let (cons bindings (cons body empty))))

(define (named-let? exp)
  (= (length exp) 4))

(define (nlet->let var binds body)
  (let ((new-binds (list var (make-lambda (let-vars binds) body))))
    (make-let new-binds
              (list var (let-vals binds)))))