(define nil '())
(define test (list 1 2 3 4 5))

; Print
(define (print x)
    (display x)
    (newline))

; Transform the list with given procedure
(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items)))))

; Return the the list that is squared from
; given list
(define (square-list items)
    (map (lambda (x) (* x x))
         items))

(print (square-list test)) ; (1 4 9 16 25)
