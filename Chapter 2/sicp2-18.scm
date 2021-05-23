(define test (list 1 2 3 4 5 9))
(define test2 (list 4 5 9 3 7))

(define (print line)
    (display line)
    (newline))

; man this is so hard, recursive solution i searched
(define (reverse-recur items)
    (if (null? items)
        '()
        (append (reverse-recur (cdr items))
                (cons (car items) '()))))

; Iterative reverse given list
(define (reverse items)
    (define (iter a result)
        (if (null? a)
            result
            (iter (cdr a) (cons (car a) result))))
    (iter items '()))

(print (reverse test))
(print (reverse test2))
(print (append test (list 9 10)))
(print (reverse-recur test))
(print (reverse-recur test2))
