#lang racket

(require "syntax.rkt"
         "database.rkt"
         "stream-ops.rkt"
         "qeval.rkt"
         "database.rkt")


(define input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
;    (display q)
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
;            (display "We're processing a query")
            (newline)
            (display output-prompt)
;            (display "About to process query...")
            (display-stream
              (stream-map
                (lambda (frame)
                  (instantiate-t q
                               frame
                               (lambda (v f)
                                 (contract-question-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop)))))

(query-driver-loop)

