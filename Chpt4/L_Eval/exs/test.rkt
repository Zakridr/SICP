(assert! (job john baker))
(assert! (wife john mary))
(assert! (p 4 4))
(assert! (p point))
(assert! (p happy))
(assert! (p 4 happy))
(assert! (p 4 point))
(assert! (p 6 happy))

(and (job ?x baker)
     (wife ?x ?y))

(and (p ?x 4)
     (p ?x ?z)
     (p ?w ?z))

(and-new (p ?x 4)
     (p ?x ?y)
     (p ?w ?y))

;(and (p ?x 4)
;     (p ?x 10))

;(and-new (p ?x 4)
;     (p ?x 10))
; of course
; new implementation of and does the processing independently, so ?x is unbound
; when we pass to the lisp-value pred
;(and-new (p ?x ?y)
;     (lisp-value > ?x 2))
