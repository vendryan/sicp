; Print
(define (print line)
    (display line)
    (newline))

; Make interval
(define (make-interval upper lower)
    (cons upper lower))

; Way to get upper bound and lower bound of
; given interval
(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))

; difference of interval is just upper bound - lower bound

; procedure that get the difference of given interval
(define (sub-interval interval)
    (- (upper-bound interval)
       (lower-bound interval)))

(define test (make-interval 8 20))

(print (sub-interval test))
