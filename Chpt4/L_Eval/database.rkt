#lang racket

(provide 
  add-assertion!
  add-rule!
  add-rule-or-assertion!
  binding-in-frame
  binding-value
  binding-variable
  extend
  fetch-assertions
  fetch-rules
  get
  get-all-assertions
  get-all-rules
  put)

(require "stream-ops.rkt"
         "syntax.rkt")

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

; ? indexes all patterns that start with a variable
; indexing scheme: look at the start of the assertion/rule conclusion

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream)))

(define (add-rule-or-assertion! assertion)
  (if (rule? assertion)
    (add-rule! assertion)
    (add-assertion! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertions THE-ASSERTIONS))
    (set! THE-ASSERTIONS
      (stream-cons assertion old-assertions))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES
      (stream-cons rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream
              (get-stream key 'assertion-stream)))
        (put key
             'assertion-stream
             (stream-cons assertion
                          current-assertion-stream))))
    'not-indexable))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
        (let ((current-rule-stream
                (get-stream key 'rule-stream)))
          (put key
               'rule-stream
               (stream-cons rule
                            current-rule-stream))))
      'not-indexable)))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

; frames and bindings

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding)
  (car binding))

(define (binding-value binding)
  (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))

; procs for the table - data-directed programming

(define table (make-hash))

(define (put key1 key2 item)
  (hash-set! table (cons key1 key2) item))

(define (get key1 key2)
  (hash-ref table (cons key1 key2) false))

; 4.70
; the purpose of the let bindings in the procedures add-(assertion|rule)!
; is to keep a pointer to the old data structure around.
; in the alternate defn:
;;(define (add-assertion! assertion)
;  (store-assertion-in-index assertion)
;  (set! THE-ASSERTIONS
;    (stream-cons assertion THE-ASSERTIONS))
;  'ok)
; because of lazy evaluation, we don't evaluate the second argument
; to stream-cons right away, and the THE-ASSERTIONS becomes an infinite
; stream of the assertion just added...
