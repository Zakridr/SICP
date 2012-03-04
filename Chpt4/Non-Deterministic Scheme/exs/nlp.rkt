(define (non-empty-els lst)
  (cond ((null? lst) 
         '())
        ((null? (car lst))
         (non-empty-els (cdr lst)))
        (else
          (cons (car lst)
                (non-empty-els (cdr lst))))))

(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define adjectives '(adjective angry lazy sleepy happy bashful egregious gregarious))
(define adverbs '(adverb happily sleepily quickly))
(define prepositions '(prep for to in by with))

(define (parse-sentence)
  (list 'sentence
        (parse-simple-noun-phrase)
        (parse-word verbs)))
;        (parse-verb-phrase)))
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))
;(define (parse-simple-noun-phrase)
;  (define (maybe-adj article)
;    (ramb (list 'simple-noun-phrase
;               article
;               (parse-word adjectives)
;               (parse-word nouns))
;         (list 'simple-noun-phrase
;               article
;               (parse-word nouns))))
;  (maybe-adj (parse-word articles)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (ramb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (ramb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

;(define (parse-verb-phrase)
;  (let ((adverb (ramb (parse-word adverbs)
;                     '()))
;        (verb (parse-word verbs))
;        (pp (ramb '()
;                 (parse-prepositional-phrase))))
;    (non-empty-els (list 'verb-phrase
;                         adverb
;                         verb
;                         pp))))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;(define (parse-word word-list)
;  (assert (not (null? *unparsed*)))
;  (assert (memq (car *unparsed*) (cdr word-list)))
;  (let ((found-word (car *unparsed*)))
;    (set! *unparsed* (cdr *unparsed*))
;    (list (car word-list) found-word)))

(define (parse-word word-list)
  (define (select-in lst)
    (assert (not (null? lst)))
    (ramb (car lst)
         (select-in (cdr lst))))
  (let ((word-choice (select-in (cdr word-list))))
    (list (car word-list)
          word-choice)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
;    (assert (null? *unparsed*))
    sent))

(parse '(the sleepy cat))
try-again
try-again
try-again
try-again
try-again
try-again

;(parse '(the sleepy professor lectures to the gregarious student in the egregious class with the cat))
;try-again
;try-again
;try-again
;try-again
;try-again



; 4.47
;(define (parse-verb-phrase)
;  (ramb (parse-word verbs)
;       (list 'verb-phrase
;             (parse-verb-phrase)
;             (parse-prepositional-phrase))))

; we don't get a tagged list if we follow the first path
; of the ramb.
; If we interchange the order of arguments in the ramb, we fall into an 
; infinite loop, since the ramb chooses from left to right so we never
; exit the recursion.
; This gives us the rule: VP := v | VP PP
; original rule was: VP := v | v PP
; Doesn't allow VPs to contain arbitrary PPs as tail!

; 4.48
; added adjectives and adverbs
