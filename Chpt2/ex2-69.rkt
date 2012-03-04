(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

; selectors
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;decoder
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit - CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ;symbol
                               (cadr pair))  ;frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (word-builder char tree-er word)
    (if (leaf? tree-er)
        (if (eq? char (symbol-leaf tree-er)) word '())
        (let ((left-result (word-builder char (left-branch tree-er) (append word '(0))))
          (right-result (word-builder char (right-branch tree-er) (append word '(1)))))
      (cond ((not (eq? '() left-result)) left-result)
            ((not (eq? '() right-result)) right-result)
            (else '())))))
  (let ((result (word-builder symbol tree '())))
    (if (eq? '() result) "Char not found" result)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (define (get-last-pair x) (list-tail x (- (length x) 2)))
  (define (get-all-but-last-pair x) (reverse (list-tail (reverse x) 2)))
  (cond ((= 1 (length leaf-set)) leaf-set)
        ((= 0 (length leaf-set)) "ERR - successive-merge operating on null list")
        ((= 2 (length leaf-set)) (make-code-tree (car leaf-set) (cadr leaf-set)))
        (else
         (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))))
        

        
    

; test stuff

; 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

(encode '(A D A B B C A) sample-tree)
(define test-set '((A 4) (B 2) (C 1) (D 1)))

(define test-tree (generate-huffman-tree test-set))
(define rock-set '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
(define rock-tree (generate-huffman-tree rock-set))
(define rock-msg '(get a job sha na na na na na na na na get a job sha na na na na na na na na wah yip yip yip yip yip yip yip yip yip Sha boom))

(length (encode rock-msg rock-tree))
(symbols rock-tree)

; 2.71 setup: frequencies are 2^(n-1), ..., 1
; 2.71: 1 bit for most frequent, n-1 bits for least frequent

; 2.72: Orders of growth: O(1) for most frequent, O(n) for least frequent