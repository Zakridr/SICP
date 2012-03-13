(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))

(assert! (address (Warbucks Oliver) (Swellesly (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull Dewitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull Dewitt) (administration secretary)))
(assert! (salary (Aull Dewitt) 25000))
(assert! (supervisor (Aull Dewitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer) (computer programmer trainee)))
(assert! (can-do-job (administration secretary) (administration big wheel)))

; 4.55
; 1 : all supervised by Ben Bitdiddle
;(supervisor ?x (Bitdiddle Ben))

;2 all accountants
;(job ?person (accounting . ?subtype))
;
;;3 all Slumerville inhabitants
;(address ?person (Slumerville . ?add))
;
;;compound queries
;
;(and (job ?person (computer programmer))
;     (address ?person ?where))
;
;(or (supervisor ?x (Bitdiddle Ben))
;    (supervisor ?x (Hacker Alyssa P)))
;
;(and (supervisor ?x (Bitdiddle Ben))
;     (not (job ?x (computer programmer))))
;
;(and (salary ?person ?amount)
;     (lisp-value > ?amount 30000))
;
;; 4.56
;;a. supervised by Ben Bitddile w/ address
;(and (supervisor ?person (Bitdiddle Ben))
;     (address ?person ?where))
;
;;b all peopple with salary less than BB's, along with their salary and BB's
;(and (salary (Bitdiddle Ben) ?bb-sal)
;     (salary ?x ?x-sal)
;     (lisp-value > ?bb-sal ?x-sal))
;
;;c all ppl supervised by someone outside of comp division
;(and (supervisor ?s-ee ?s-or)
;     (not (job ?s-or (computer . ?type)))
;     (job ?s-or ?job))
;
;; rules
;
(assert! (rule (same ?x ?x)))
(assert! (rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2)))))
;
;;(lives-near ?x ?y)
;
;(assert! (rule (wheel ?person)
;      (and (supervisor ?middle-manager ?person)
;           (supervisor ?x ?middle-manager))))
;
;(wheel ?who)
;; general rule form: (rule <conclusion> <body>)
;
;(rule (outranked-by ?staff-person ?boss)
;      (or (supervisor ?staff-person ?boss)
;          (and (supervisor ?staff-person ?middle-manager)
;               (outranked-by ?middle-manager ?boss))))
;
;(rule (dork ben-bit))
;(dork ?x)
;
;(outranked-by ?x ?y)
;;
;;; 4.57
;(can-do-job ?x ?y)
;(can-do-job (computer wizard) ?x)
(assert! (rule (can-replace ?p1 ?p2)
      (and (and (job ?p1 ?j1)
                (job ?p2 ?j2)
                (or (can-do-job ?j1 ?j2)
                    (same ?j1 ?j2)))
           (not (same ?p1 ?p2)))))
(can-replace ?x (Fect Cy D))
(and (can-replace ?p1 ?p2)
     (salary ?p1 ?s1)
     (salary ?p2 ?s2)
     (lisp-value < ?s1 ?s2))
;
;; 4.58
;
(assert! (rule (big-shot ?person ?div)
      (and (job ?person (?div . ?type))
           (not (and (supervisor ?person ?manager)
                     (job ?manager (?div . ?other)))))))
;(big-shot ?x ?y)
;
;; 4.59
;(meeting accounting (Monday 9am))
;(meeting administration (Monday 10am))
;(meeting computer (Wednesday 3pm))
;(meeting administration (Friday 1pm))
;(meeting whole-company (Wednesday 4pm))
;;a
;(meeting ?div (Friday ?time))
;;b
;(rule (meeting-time ?person ?day-and-time)
;      (or (meeting whole-company ?day-and-time)
;          (and (job ?person (?div . ?type)) 
;               (meeting ?div ?day-and-time))))
;; c
;(meeting-time (Hacker Alyssa P) ?day-and-time)
;
;; 4.60
;; duplication of results:
;(lives-near ?p1 ?p2)
;; is there a method to avoid redundancy here?
;
;
;
;; 4.64
;(assert! (rule (outranked-by ?staff ?boss)
;      (or (supervisor ?staff ?boss)
;          (and (outranked-by ?middle-manager ?boss)
;               (supervisor ?staff
;                           ?middle-manager)))))
;
;(outranked-by (Bitdiddle Ben) ?who)
;
;; This enters an infinite loop, becuase the first branch of the and
;; matches no assertions, but it does match the rule.
;
;; From the beginning:
;; Go through assertions, find nothing.
;; Go through rules, find this rule. Do the first branch of the or in //.
;; As for the second branch of the or, first branch of and, when we 
;; unify 
;; (outranked-by (Bitdiddle Ben) ?who) with
;; (outranked-by ?middle-manager ?boss), we just get the original pattern
;; back, more or less. This then goes through again... and again...
;
;; 4.65
;(wheel ?who)
;
;(wheel (Warbucks Oliver))
;(wheel (Bitdiddle Ben))
;(wheel (Warbucks Oliver))
;(wheel (Warbucks Oliver))
;(wheel (Warbucks Oliver))
;
;(rule (wheel ?person)
;      (and (supervisor ?middle-manager ?person)
;           (supervisor ?x ?middle-manager)))
;
;; Trace through execution. Warbucks supervises BB, who supervises 3 people
;; -> his name comes up three times there.
;; Warbucks supervises Scrooge who supervises Cratchet -> his name comes
;; up again.
;; BB only has one minion w/ a minion: Hacker who supervises Louis.
;
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

;(?rel Adam Irad)
