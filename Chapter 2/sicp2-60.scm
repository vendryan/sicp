(define (print line)
    (display line)
    (newline))

; Set representation as unordered list
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (cons x set))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2))
          '())
        ((element-of-set? (car set1) set2)
          (cons (car set1) (intersection-set (cdr set1)
                                             set2)))
        (else (intersection-set (cdr set1) set2))))
(define (union-set set1 set2)
  (append set1 set2))

(define test (list 2 3 2 1 3 2 2))
(define test2 (list 2 3 4 1 3 4 2 3 4 4))
(print (intersection-set test test2))
(print (union-set test test2))

; My opinion
; For adjoin-set and union-set it become much faster beacuse
; you just cons/append set1 and set2 and hence have 
; run in O(1) but for intersection-set and element-of-set
; much worse because more item means longer to scan all of them
