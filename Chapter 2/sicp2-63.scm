(define (print line)
  (display line)
  (newline))

; Represent binary tree as list with
; first item is the value, second is the left branch
; and third is right branch
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
          (element-of-set? x (left-branch set)))
        ((> x (entry set))
          (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
          (make-tree (entry set)
                     (adjoin-set x (left-branch set))
                     (right-branch set)))
        ((> x (entry set))
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

(define tree1 (make-tree 1 '() '()))
(define tree7 (make-tree 7 '() '()))
(define tree11 (make-tree 11 '() '()))
(define tree3 (make-tree 3 tree1 '()))
(define tree9 (make-tree 9 tree7 tree11))
(define tree5 (make-tree 5 tree3 tree9))

(define tree1-2 (make-tree 1 '() '()))
(define tree11-2 (make-tree 11 '() '()))
(define tree5-2 (make-tree 5 '() '()))
(define tree9-2 (make-tree 9 '() tree11-2))
(define tree7-2 (make-tree 7 tree5-2 tree9-2))
(define tree3-2 (make-tree 3 tree1-2 tree7-2))

(print (element-of-set? 10 tree5))
(print (adjoin-set 4 tree5))
