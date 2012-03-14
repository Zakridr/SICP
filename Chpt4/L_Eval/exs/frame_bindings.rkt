(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append (find-assertions query-pattern frame)
                     (apply-rules query-pattern frame)))
    frame-stream))

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunctino? disjuncts)
    the-empty-stream
    (interleave
      (qeval (first-disjunct disjuncts) frame-stream)
      (disjoin (rest-disjuncts disjuncts) frames-tream))))

; If applying a rule will cause the qeval to enter a loop, then the qeval with
; these modified defns will not return any results, even if the given pattern
; with the frame bindings matches an assertion in the database.
; Makes qeval more susceptible to loops!

; 4.72
; disjoin and stream-flatmap use interleave to cover the case of infinite
; streams

; 4.73

(define (flatten-stream stream)
  (if (stream-empty? stream)
    the-empty-stream
    (interleave
      (stream-first stream)
      (flatten-stream (stream-rest stream)))))

; the call to interleave here strictly evaluates the (flatten-stream...)
; subexpression, instead of lazily evaluating it as originally defined.
; this allows this function to return if we have an infinite stream to flatten

; 4.74
; b. I don't think the new procs change the behaviour...
; The order of output is not changed, because a stream of trivial streams
; (1 or 2 elements) will not trigger the 'switch' in interleave
