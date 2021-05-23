; Make interval
(define (make-interval upper lower)
    (cons upper lower))

; Way to get upper bound and lower bound of
; given interval
(define (lower-bound interval)
    (car interval))

(define (upper-bound interval)
    (cdr interval))
