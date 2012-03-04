#lang racket


;environment: sequence of frames
;frame: table (possibly empty) of bindings - associate variable names with their values
; each frame also has a pointer to its enclosing environment
(define (make-fund balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request - MAKE-ACCOUNT" m))))
  dispatch)
    
(define (make-authenticator pass) 
  (lambda (attempt)
    (if (eq? attempt pass) #t #f)))

(define (make-auth-fund auth fund)
  (lambda (attempt m)
    (if (auth attempt)
        (if (eq? m 'get-fund)
            fund
            (fund m))
        "Incorrect password")))

(define (make-account balance password)
  (make-auth-fund (make-authenticator password)
                  (make-fund balance)))

(define (make-joint old-auth-fund old-pass new-pass)
  (let ((result (old-auth-fund old-pass 'get-fund)))
    (if (not (eq? result "Incorrect password"))
         (make-auth-fund (make-authenticator new-pass)
                         result)
        "Failed to make joint account")))

  

;need to do that damn problem...

; how it should work (account maker)
(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)

((peter-acc 'open-sesame 'deposit) 10)
((peter-acc 'open-sesame 'deposit) 10)

(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))
paul-acc
((paul-acc 'rosebud 'withdraw) 10)
((peter-acc 'open-sesame 'withdraw) 10)
((paul-acc 'rosebud 'withdraw) 10)
((peter-acc 'open-sesame 'withdraw) 10)
