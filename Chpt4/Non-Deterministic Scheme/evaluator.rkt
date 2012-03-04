#lang racket

(require "eval-data-structs.rkt"
         "syntax-spec.rkt")

(provide ambeval)

; general form of execution procedure:
; λ env succeed fail . <body>
; success continuation: λ value fail . <body>
; fail continuation: λ. <body>

; added two additional arguments for continuations
(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((permanent-assignment? exp) (analyze-p-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((if-fail? exp) (analyze-if-fail exp))
        ((lambda? exp)
         (analyze-lambda exp))
        ((begin? exp)
         (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((amb? exp) (analyze-amb exp))
        ((ramb? exp) (analyze-ramb exp))
        ((require? exp) (analyze-require exp))
        ((application? exp)
         (analyze-application exp))
        (else
          (error "Unknown expression type - EVAL" exp))))

; always succeed:

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail) 
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail) 
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env)
             fail)))

; potentially fail:

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)   ; success continuation
               (let ((old-value 
                       (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda () ;fail continuation
                            (set-variable-value! var
                                                 old-value
                                                 env)
                            (fail2)))))
             fail))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation to evaluate predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             fail))))

; always succeeds

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env)
               fail))))

; maybe fails

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure cont for calling a
         fail)))
  ;      (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence - ANALYZE")
      (loop (car procs) (cdr procs)))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs
                         env
                         (lambda (args fail3)
                           (execute-application
                             proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs) env
                  ;; success cnt
                  (lambda (arg fail2)
                    (get-args (cdr aprocs)
                              env
                              ;; success cnt for 
                              ;; recursive call to get-args
                              (lambda (args fail3)
                                (succeed (cons arg args)
                                         fail3))
                              fail2))
                  fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args)
                  fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
          (error "Unknown procedure type - EXECTUE-APPLICATION"
                 proc))))

; analyze-amb

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env
                         succeed
                         (lambda ()
                           (try-next (cdr choices))))))
      (try-next cprocs))))

; analyze-ramb
; 4.50

(define (analyze-ramb exp)
  (define (truncate-list lst pos)
    (append (take lst pos)
            (drop lst (+ pos 1))))
  (let ((cprocs (map analyze (ramb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          (let* ((bd (length choices))
                 (choice-num (random bd))
                 (choice (list-ref choices choice-num))
                 (rem-choices (truncate-list choices choice-num)))
            (choice env succeed
                    (lambda ()
                      (try-next rem-choices))))))
      (try-next cprocs))))

; permanent assignment
;  4.51

(define (analyze-p-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)   ; success continuation
               (set-variable-value! var val env)
               (succeed 'ok fail2))
             fail))))

; if-fail construct
; 4.52
(define (analyze-if-fail exp)
  ; evaluate first expression
  ; use the above as a guide...
  (let ((testproc (analyze (test-expression exp)))
        (altproc (analyze (alt-expression exp))))
    (lambda (env succeed fail)
      (testproc env
                ; success continuation
                (lambda (val fail2)
                  (succeed val fail2))
                ; fail continuation
                (lambda ()
                  (altproc env succeed fail))))))

; require special form
; 4.54
(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                 (fail)
                 (succeed 'ok fail2)))
             fail))))
