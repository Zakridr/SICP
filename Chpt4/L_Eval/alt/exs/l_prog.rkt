(assert! (rule (same ?x ?x)))
(assert! (rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2)))))
;; 4.66
;; new syntax:
;; (accumulation-function <var> <query pattern>)
;; Ben has realized that the system will return redundant patterns!
;; So the statistics will be off by overcounting some things.
;; He can salvage his scheme by first passing the stream of frames 
;; (output of query) through a filter which removes redundant matches,
;; and then passing it through the map function.
;
;; 4.67
;; To avoid loops:
;; Keep a history of (pattern(s), frame) pairs. Each time we make a
;; deduction - unify a pattern - we check to see if this pattern
;; we're unifying (loop through history list), with these 
;; bindings (the frame) has been seen before.
;; If so, then we've backtracked, so we stop where we are.
;
;; 4.68
;; implement (reverse) rule
;(rule (reverse () ()))
;
(assert! (rule (append-to-form () ?y ?y)))

(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z)))

(append-to-form ?x ?y (1 2 3 4))

(assert! (rule (reverse (?u . ?v) ?z)
               (and (reverse ?v ?r)
                    (append-to-form ?r (?u) ?z))))

(assert! (rule (reverse () () )))
(?x () () ?z)

(reverse (1 2 3) ?x)

; Only recurses on the first element, I need first guy to be defined

;(reverse (1) ?x)

;(reverse (1 2 3) ?x)
;(reverse ?x (1 2 3))
;
;; 4.69
;; not allowed...
;(rule ((?first . ?rest grandson) ?gg ?s)
;      (and ((?rest grandson) ?gg ?f)
;           (grandson ?f ?s)))
;
;; the above is no good...
;; determine whether the last element of a list is 'son
;; perhaps?
;(assert! (rule (ends-in-x ?x (?x))))
;(assert! (rule (ends-in-x ?x (?car . ?cdr))
;      (ends-in-x ?x ?cdr)))
(assert! (rule (ends-in-grandson ?x)
      (and (reverse ?x ?r)
           (same ?r (?u . ?v))
           (same ?u grandson))))

(assert! (rule (last ?lst ?final)
               (and (reverse ?lst ?r)
                    (same ?r (?u . ?v))
                    (same ?u ?final))))

; directly:
(assert! (rule (alt-last (?car . ?cdr) ?final)
               (or (and (same ?cdr (?final)))
                   (alt-last ?cdr ?final))))


(assert! (son Adam Cain) )
(assert! (son Cain Enoch) )
(assert! (son Enoch Irad) )
(assert! (son Irad Mehujael) )
(assert! (son Mehujael Methushael) )
(assert! (son Methushael Lamech) )
(assert! (wife Lamech Ada))

(assert! (son Ada Jabal) )
(assert! (son Ada Jubal))

(ends-in-grandson (1 2 3 grandson))
(last (1 2 3 4) ?x)
(alt-last (1 2) ?x)
;(alt-last ?lst 1)
;
(assert! (rule ((grandson) ?g ?s)
      (and (son ?f ?s)
           (son ?g ?f))))

(assert! (rule ((great . ?rel) ?x ?y)
      (and (?rel ?z ?y)
           (son ?x ?z))))

(grandson ?x ?y)
((great grandson) ?g ?ggs)

(?rel Adam Irad)
