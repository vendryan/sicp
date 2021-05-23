(define test (list 1 2 3 4 5 9))
(define test2 (list 4 5 9 3 7))

(define (print line)
    (display line)
    (newline))

; Iterative reverse given list
(define (reverse items)
    (define (iter a result)
        (if (null? a)
            result
            (iter (cdr a) (cons (car a) result))))
    (iter items '()))

(print (reverse test)) ; 9
(print (reverse test2)) ; 7
