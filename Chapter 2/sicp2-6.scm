; This known as Church numeral
; zero means apply given procedure to given number zero times
; one means apply given procedure to given number one times
; and so on ...

(define zero 
    (lambda (f) (lambda (x) x)))

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

(define one
    (lambda (f) (lambda (x) (f x))))

(define two
    (lambda (f) (lambda (x) (f (f x)))))

(define inc
    (lambda (x) (+ x 1)))

(display ((two inc) 2)) ; apply inc to 2 two times

; make a procedure that sum two church numeral
(define (sum-church a b)
    (lambda (f) (lambda (x) ((a f) ((b f) x)))))

(define three (add-1 two))

(define five (sum-church two three))

(newline)
(display ((five inc) 2)) ; apply inc five times to 2
