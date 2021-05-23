; if cdr of the list is empty list then return the car of the list
; Otherwise return the last pair of cdr of the list
(define test (list 1 2 3 4 5 9))
(define test2 (list 4 5 9 3 7))

(define (print line)
    (display line)
    (newline))

(define (last-pair items)
    (if (null? (cdr items))
        (car items)
        (last-pair (cdr items))))

(print (last-pair test)) ; 9
(print (last-pair test2)) ; 7
