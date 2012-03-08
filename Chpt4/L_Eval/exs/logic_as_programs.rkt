; append rules
(append-to-form x y z)

(rule (append-to-form () ?y ?y))

(rule (append-to-form (?u . ?v) ?y (?u . ?z))
      (append-to-form ?v ?y ?z))

; 4.61
(rule (?x next-to ?y in (?x ?y . ?u)))

(rule (?x next-to ?y in (?v . ?z))
      (?x next-to ?y in ?z))

; response to the following?

(?x next-to ?y in (1 (2 3) 4))

; wrong order...?
(4 next-to () in (1 (2 3) 4))
((2 3) next-to 4 in (1 (2 3) 4))
(1 next-to (2 3) in (1 (2 3) 4))


(?x next-to 1 in (2 1 3 1))
(2 next-to 1 in (2 1 3 1))
(3 next-to 1 in (2 1 3 1))

; 4.62

; last pair operation
(rule (last-pair (?x) ?x))

(rule (last-pair (?u . ?v) ?x)
      (last-pair ?v ?x))


; 4.63
(son Adam Cain) 
(son Cain Enoch) 
(son Enoch Irad) 
(son Irad Mehujael) 
(son Mehujael Methushael) 
(son Methushael Lamech) 
(wife Lamech Ada)

(son Ada Jabal) 
(son Ada Jubal)

; if S son of f, f son of G, S grandson of G
(rule (grandson ?g ?s)
      (and (son ?f ?s)
           (son ?g ?f)))

(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son ?w ?s)))

;grandson of cain:
(grandson Cain ?s)

(son Lamech ?s)

