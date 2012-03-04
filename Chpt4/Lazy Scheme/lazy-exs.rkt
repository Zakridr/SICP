#lang racket

; 4.27

; (define w (id (id 10)))
; the outer id is called on '(thunk id 10). Since it returns its input,
; it doesn't force the thunk. Input is returned to define, which doesn't force it.
; the REPL gets 'ok, so the thunk is never forced

; -> count is 1.

; w
; now the REPL needs to force the thunk, so we evaluate (id 10).
; Results in counter getting incremented again. 
; w -> 10
; count -> 2

; 4.28

; we need to force the evaluator in the following case:
; (define (i x) x)
; ((i a) b)
; (i a) returns ('thunk a), which is no good at all!

; 4.29

; w/ memoization: 100, 1
; w/o memoization: 100, 2

; 4.30
; a. The values are printed as expected because when eval-sequence is called on the body
; of the expression λx. (newline) (display x),
;   the evaluator comes across primitive procs which follow applicative order evaluation.

; b.
; (p1 1) -> (1 2)
; (p2 1) -> 1
; with Cy's change, we'll force each expression in a begin-sequence, so the 
; (set! x (cons x '(2))) will be evaluated, as opposed to just thunked.
; Hence it (1 2) will be returned

; c. each value is forced already because they are applications of a primitive,
; so it makes no difference to force them explicitly in eval-sequence

; d. The lazy evaluator makes Scheme more 'functional'. Trying to stretch the evaluator
; to deal well with side effects is misguided. This may also result in many needless
; computations being performed

; 4.31