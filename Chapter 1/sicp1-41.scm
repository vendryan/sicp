(define (double f)
    (lambda (x) (f (f x))))
(define (inc x)
    (+ x 1))

; Hard to use substitition model but you will eventually get
; (double (double (double (double inc)))) which is
; increment by 16
(display (((double (double double)) inc) 2)) ; 21
(newline)
(display ((double inc) 3))
(newline)
(display ((lambda (x) (+ x 5)) ((lambda (x) (+ x 5)) 5)))
