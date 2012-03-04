#lang racket

; needs to deal with extra message, 'authenticate
(define (init-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (lambda (message)
    (cond ((eq? message 'withdraw) withdraw)
          ((eq? message 'deposit) deposit)
          ((eq? message 'authenticate) #t)
          (else (error "Unknown request - INIT-ACCOUNT" message)))))



; no this guy should deal with extra message
(define (make-accessor acc set-pass)
  (lambda (pass message)
    (if (eq? set-pass pass)
        (if (eq? message 'authenticate)
            "New account created."
            (acc message))
        (lambda (n) "Incorrect password"))))

(define (make-account balance password)
  (define acc (init-account balance))
  (make-accessor acc password))

(define (make-joint acc pass new-pass)
  (if (acc pass 'authenticate)
      (make-accessor acc new-pass)
      "Cannot make joint account. Incorrect password"))

; how it should work (account maker)
(define peter-acc (make-account 100 'open-sesame))
((peter-acc 'open-sesame 'withdraw) 40)

((peter-acc 'open-sesame 'deposit) 10)

(define paul-acc (make-joint peter-acc 'argh 'rosebud))
( (paul-acc 'rosebud 'withdraw) 40)

;( (


;'open-sesame
; joint-account maker