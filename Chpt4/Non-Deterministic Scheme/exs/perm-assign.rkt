(define count 0)

(let ((x (an-element-of '(a b c)))
      (y (an-element-of '(a b c))))
  (permanent-set! count (+ count 1))
  (assert (not (eq? x y)))
  (list x y count))

try-again
try-again
try-again
