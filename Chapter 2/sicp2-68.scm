(define (print line)
  (display line)
  (newline))

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

(define (exist-symbol? x symbol-list)
  (cond ((null? symbol-list) #f)
        ((eq? x (car symbol-list)) #t)
        (else (exist-symbol? x (cdr symbol-list)))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
                  
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))
                  
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
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))
      
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
            
(define (encode-symbol symbol tree)
  (define (encode-symbol-1 tree)
    (cond ((leaf? tree) '())
          ((exist-symbol? symbol (symbols (right-branch tree))) ; Check if exist in right branch
            (cons 1 (encode-symbol-1 (right-branch tree)))) ; if yes cons 1 with the rest of th eencode of right branch
          (else (cons 0 (encode-symbol-1 (left-branch tree)))))) ; otherwise it must exist in left branch so cons 0 with rest of the encode
    
  (if (not (exist-symbol? symbol (symbols tree)))
      (error "bad symbol -- ENCODE-SYMBOL" symbol)
      (encode-symbol-1 tree)))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(print (decode sample-message sample-tree))
(print (encode '(A D A B B C A) sample-tree))
; Result is (0 1 1 0 0 1 0 1 0 1 1 1 0)
