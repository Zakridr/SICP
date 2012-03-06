(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))

(address (Warbucks Oliver) (Swellesly (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull Dewitt) (Slumerville (Onion Square) 5))
(job (Aull Dewitt) (administration secretary))
(salary (Aull Dewitt) 25000)
(supervisor (Aull Dewitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))
(can-do-job (computer programmer) (computer programmer trainee))
(can-do-job (administration secretary) (administration big wheel))

; 4.55
; 1 : all supervised by Ben Bitdiddle
(supervisor ?person (Ben Bitdiddle))

;2 all accountants
(job ?person (accounting . ?subtype))

;3 all Slumerville inhabitants
(address ?person (Slumerville . ?add))

;compound queries

(and (job ?person (computer programmer))
     (address ?person ?where))

(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

; 4.56
;a. supervised by Ben Bitddile w/ address
(and (supervisor ?person (Ben Bitddile))
     (address ?person ?where))

;b all peopple with salary less than BB's, along with their salary and BB's
(and (salary (Ben Bitddile) ?bb-sal)
     (salary ?x ?x-sal)
     (lisp-value > ?bb-sal ?x-sal))

;c all ppl supervised by someone outside of comp division
(and (supervisor ?s-ee ?s-or)
     (not (job ?s-or (computer . ?type)))
     (job ?s-or ?job))

; rules

(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

; general rule form: (rule <conclusion> <body>)

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (outranked-by ?middle-manager ?boss))))

; 4.57
(rule (can-replace ?p1 ?p2)
      (and (and (job ?p1 ?j1)
                (job ?p2 ?j2)
                (or (can-do-job ?j1 ?j2)
                    (same ?j1 ?j2)))
           (not (same ?p1 ?p2))))
(can-replace ?x (Fect Cy D))
(and (can-replace ?p1 ?p2)
     (salary ?p1 ?s1)
     (salary ?p2 ?s2)
     (lisp-value < ?s1 ?s2))

; 4.58

(rule (big-shot ?person ?division)
      (and (job ?person (?div . ?type))
           (not (and (supervisor ?person ?manager)
                     (job ?manager (?div . ?other))))))

; 4.59
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))
;a
(meeting ?div (Friday ?time))
;b
(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (job ?person (?div . ?type)) 
               (meeting ?div ?day-and-time))))
; c
(meeting-time (Hacker Alyssa P) ?day-and-time)

; 4.60
; duplication of results:
(lives-near ?p1 ?p2)
; is there a method to avoid redundancy here?



; 4.64
(rule (outranked-by ?staff ?boss)
      (or (supervisor ?staff ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff
                           ?middle-manager))))

(outranked-by (Bitdiddle Ben) ?who)

; This enters an infinite loop, becuase the first branch of the and
; matches no assertions, but it does match the rule.

; From the beginning:
; Go through assertions, find nothing.
; Go through rules, find this rule. Do the first branch of the or in //.
; As for the second branch of the or, first branch of and, when we 
; unify 
; (outranked-by (Bitdiddle Ben) ?who) with
; (outranked-by ?middle-manager ?boss), we just get the original pattern
; back, more or less. This then goes through again... and again...

; 4.65
(wheel ?who)

(wheel (Warbucks Oliver))
(wheel (Bitdiddle Ben))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))
(wheel (Warbucks Oliver))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

; Trace through execution. Warbucks supervises BB, who supervises 3 people
; -> his name comes up three times there.
; Warbucks supervises Scrooge who supervises Cratchet -> his name comes
; up again.
; BB only has one minion w/ a minion: Hacker who supervises Louis.

; 4.66
; new syntax:
; (accumulation-function <var> <query pattern>)
; Ben has realized that the system will return redundant patterns!
; So the statistics will be off by overcounting some things.
; He can salvage his scheme by first passing the stream of frames 
; (output of query) through a filter which removes redundant matches,
; and then passing it through the map function.

; 4.67
; To avoid loops:
; Keep a history of (pattern(s), frame) pairs. Each time we make a
; deduction - unify a pattern - we check to see if this pattern
; we're unifying (loop through history list), with these 
; bindings (the frame) has been seen before.
; If so, then we've backtracked, so we stop where we are.

; 4.68
; implement (reverse) rule
(rule (reverse () ()))

(rule (reverse (?u . ?v) (?z . ?w))
      (or (and (append-to-form ?r ?u (?z . ?w))
               (reverse ?v ?r))
          (and (append-to-form ?l ?z (?u . ?v))
               (reverse ?l ?w))))

; 4.69
; not allowed...
(rule ((?first . ?rest grandson) ?gg ?s)
      (and ((?rest grandson) ?gg ?f)
           (grandson ?f ?s)))

(
