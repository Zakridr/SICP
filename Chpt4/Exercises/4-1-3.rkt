#lang racket

(require "../syntax-spec.rkt")

; 4.11

; use the same environment procs

; frame manipulation:

(define (add-binding-to-frame! var val frame)
  (set! frame (cons (mcons var val) 
                    frame)))

(define (make-frame vars vals)
  (if (empty? var-lst)
      empty
      (cons (mcons (car var-lst)
                   (car val-lst))
            (make-bindings (cdr var-lst)
                           (cdr val-lst)))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "Number of values != Number of variables" vars vals)))

(define (scan-frame var frame)
  (cond ((empty? frame) false)
        ((eq? var (mcar (car frame)))
         (car frame))
        (else
         (scan (cdr frame) var))))

(define (scan-env var env)
  (cond ((eq? the-empty-environment env)
         false)
        (or (scan-frame (car env) var)
            (scan-env (cdr env) var))))

(define (lookup-variable-value var env)
  (let ((binding (scan-env var env)))
    (if binding
        (cdr binding)
        (error "Unbound variable" var))))

(define (set-variable-value! var val env)
  (let ((binding (scan-env var env)))
    (if binding
        (set-mcdr! binding val)
        (error "Unbound variable - SET!" var))))

(define (define-variable! var val env)
  (let* ((frame (car env))
         (binding (scan-frame var frame)))
    (if binding
        (set-mcdr! binding val)
        (add-binding-to-frame! var val frame))))

; 4.12

; done above? more or less...

; 4.13

; make-unbound! - special form for evaluator
; only remove binding in first frame of the env.

(define (undefinition? exp)
  (tagged-list? exp 'make-unbound!))

(define (eval-undefinition exp env)
  (undefine-variable! (undefinition-variable exp)
                      env))

(define (undefinition-variable exp)
  (cadr exp))

(define (split key keys vals)
  (define (split-iter key k-head k-tail v-head v-tail)
    (cond ((or (empty? k-tail) (eq? (mcar k-tail) key))
           (list (cons k-head k-tail)
                 (cons v-head v-tail)))
          (let ((new-k-head (mcons (car k-tail)))
                (new-k-tail (mcdr k-tail))
                (new-v-head (mcons (mcar v-head)))
                (new-v-tail (mcdr v-tail)))
            (split-iter key new-k-head new-k-tail
                        new-v-head new-v-tail))))
  (split-iter key empty keys empty vals))


(define (undefine-variable! var env)
  (cons (filter-frame var (car env)) (cdr env)))

; fuck so confusing, just pretend you can do everything
; with lists ie. just use car and cdr

(define (filter-frame var frame)
  (let* ((keys (frame-variables frame))
        (vals (frame-values frame))
        (result (split var keys vals))
        (k-head (caar result))
        (k-tail (cdar result))
        (v-head (caadr result))
        (v-tail (cdadr result)))
    (if (empty? k-head)
        (make-frame (cdr k-tail)
                    (cdr v-tail))
        (make-frame (cons k-head (cdr k-tail))
                    (cons v-head (cdr v-tail))))))