#lang racket

; queue stuff

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item) (set-mcar! queue item))
(define (set-rear-ptr! queue item) (set-mcdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (mcdr (front-ptr queue)))
         queue)))

;make-wire
;or-gate
;and-gate
;inverter

;(get-signal <wire>)
; returns current signal value of wire

;(set-signal! <wire> <new value>)
; changes value of signal on wire to the new value

;(add-action! <wire> <proc of no args>)
; asserts that proc should be run whenever signal on wire changes value.
; such procs are the vehicles that communicate change of signal value on wire to other wires

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        (else
         (if (and (and (<= s1 1) (>= s1 0)) (and (<= s2 1) (>= s2 0)))
             0
             (error "Invalid signal" s1 s2)))))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (cond ((or (= s1 1) (= s2 1)) 1)
        ((and (= s1 0) (= s2 0)) 0)
        (else (error "Invalid signal" s1 s2)))) 

(define (half-adder a b s c) 
  (let ((d (make-wire)) (e (make-wire)))                           
    (or-gate a b d) 
    (and-gate a b c) 
    (inverter c e) 
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out) 
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire))) 
    (half-adder b c-in s c1) 
    (half-adder a s sum c2) 
    (or-gate c1 c2 c-out) 
    'ok))

(define (ripple-carry-adder As Bs Ss Cout)
  (let ((Cin (make-wire)))
    (if (null? (cdr As))
        (set-signal! Cin 0)
        (ripple-carry-adder (cdr As) (cdr Bs) (cdr Ss) Cin))
    (full-adder (car As) (car Bs) Cin (car Ss) Cout)))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures
            (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation - WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin
        ((car procedures))
        (call-each (cdr procedures)))))

; syntactic sugar for wires
(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;agendas keep track of what's going on, and what happens next
; (make-agenda)
; (empty-agenda? <agenda>)
; (first-agenda-item <agenda>)
; (remove-first-agenda-item! <agenda>)
; (add-to-agenda! <time> <action> <agenda>)
; (current-time <agenda>)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propogate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propogate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire))
                 (newline))))

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (mcons 0 '()))

(define (current-time agenda) (mcar agenda))

(define (set-current-time! agenda time)
  (set-mcar! agenda time))

(define (segments agenda) (mcdr agenda))

(define (set-segments! agenda segments)
  (set-mcdr! agenda segments))

(define (first-segment agenda) (mcar (segments agenda)))

(define (rest-segments agenda) (mcdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                      (mcdr segments))) ; this (mcdr segments) could be rest here
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment time action)
                segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))
        '())))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty - FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))



; testing!
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (make-wires n)
  (if (> n 0)
      (cons (make-wire) (make-wires (- n 1)))
      '()))

(define (enumerate n)
  (if (> n 0)
      (cons (- n 1) (enumerate (- n 1)))
      '()))

(define (binary->decimal wires)
  (define bin (map get-signal wires))
  (define nums (enumerate (length wires)))
  (foldl (lambda (place count sum) (+ (* (expt 2 count) place) sum))
         0
         bin
         nums))


; this is my wire adder!
;values is a list of 0s and 1s
(define (set-signals! wires values)
  (for-each set-signal! wires values))

(define As (make-wires 5))
(define Bs (make-wires 5))
(define Ss (make-wires 5))
(define Cout (make-wire))
(define result-wires (cons Cout Ss))

(length As)
(define results (list 'first 'second 'third 'fourth 'fifth))
(map probe results Ss)
(probe 'carry Cout)
(ripple-carry-adder As Bs Ss Cout)
(set-signals! As (list 1 1 1 1 0))
(set-signals! Bs (list 1 1 1 1 0))
(propogate)
(binary->decimal result-wires)