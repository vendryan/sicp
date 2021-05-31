(define (print line)
    (display line)
    (newline))

; Set representation as ordered list (better than unordered
; in term of time complexity using this representation)
; if x is smaller then set item then we can
; infer that, the item is not in set and thus return false
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((> x (car set)) (element-of-set? x (cdr set)))
        ((< x (car set)) #f)))
(define (adjoin-set x set)
  (cond ((null? set) (cons x '()))
        ((= x (car set)) set)
        ((> x (car set)) (cons (car set) (adjoin-set x (cdr set))))
        ((< x (car set)) (cons x set))))
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))



(define test (list 1 2 4 6 8 10))
;(define test2 (list 2 3 4 1 3 4 2 3 4 4))
(print (element-of-set? 3 test))
(print (adjoin-set 3 test))
(print (adjoin-set 12 test))
;(print (intersection-set test test2))
;(print (union-set test test2))
